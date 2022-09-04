#ifndef LIST_MODEL_HPP
#define LIST_MODEL_HPP

#include <QString>
#include <QModelIndex>
#include <QAbstractListModel>
#include <QSortFilterProxyModel>
#include <QDebug>
#include <QFont>
#include <QColor>

#include "fuzzy_match.hpp"
#include "../../../cpp_common.hpp"


class ModelData {
    Q_GADGET
    Q_PROPERTY(QString qdata READ getQdata WRITE setQdata);
    Q_PROPERTY(int score READ getScore WRITE setScore);

  public:
    QString qdata;
    int     score;

    void    setQdata(CR<QString> _qdata) { qdata = _qdata; }
    QString getQdata() const { return qdata; }
    void    setScore(int _score) { score = _score; }
    int     getScore() const { return score; }
};

using ScoreVec = Vec<ModelData>;

Q_DECLARE_METATYPE(ModelData*);

struct NimProcArg {
    QString name;
    QString type;
};

Q_DECLARE_METATYPE(NimProcArg);

struct NimProc {
    int                 id;
    QString             name;
    QString             rettype;
    QVector<NimProcArg> args;
    QString             docstring;

    inline QString getSearchName() {
        QString res;
        res += name;
        res += "(";
        for (auto& arg : args) {
            res += arg.name + ": " + arg.type + ", ";
        }
        res += "): " + rettype;

        return res;
    }
};

Q_DECLARE_METATYPE(NimProc);

class ListItemModel : public QAbstractListModel {
    ScoreVec items;

  public:
    void setItems(CR<Vec<QString>> _items) {
        beginResetModel();

        items.clear();
        items.reserve(_items.size());
        for (const auto& it : _items) {
            items.push_back({it, -1});
        }

        endResetModel();
    }

    ScoreVec* getScores() { return &items; }

    void updateScores(QString pattern) {
        for (size_t i = 0; i < items.size(); ++i) {
            items[i].score = fts::get_score(
                items[i].qdata.data(), pattern.data());
        }
    }

  public:
    int rowCount(const QModelIndex& parent
                 [[maybe_unused]]) const override {
        return items.size();
    }

    QVariant data(const QModelIndex& index, int role [[maybe_unused]])
        const override {
        if (role == Qt::DisplayRole) {
            return items[index.row()].qdata;
        } else {
            return QVariant();
        }
    }
};

class FuzzySearchProxyModel : public QSortFilterProxyModel {
    Q_OBJECT
    ScoreVec*      scores;
    ListItemModel* list;
    int            maxItemShowed = 50;
    bool           initDone      = false;

  public:
    Q_INVOKABLE void updateScores(QString pattern) {
        list   = dynamic_cast<ListItemModel*>(sourceModel());
        scores = list->getScores();

        list->updateScores(pattern);
        this->invalidateFilter();

        initDone = true;
    }

    Q_INVOKABLE void sortOnPattern(QString _patt) {
        updateScores(_patt);
        sort(0);
    }

    Q_INVOKABLE ModelData* getModelData(int index) {
        return &scores->at(index);
    }

    void setMaxItemShowed(int value);

    Q_INVOKABLE int getRowCount() const { return rowCount(); }

  protected:
    bool lessThan(
        const QModelIndex& source_left,
        const QModelIndex& source_right) const override {

        return scores->at(source_left.row()).score >
               scores->at(source_right.row()).score;
    }

    inline bool filterAcceptsRow(
        int                source_row,
        const QModelIndex& source_parent [[maybe_unused]]) const override {
        if (initDone) {
            return 0 < scores->at(source_row).score;

        } else {
            return source_row < maxItemShowed;
        }
    }

  public:
    inline QVariant data(const QModelIndex&, int) const override {

        return QVariant();
    }
};

#endif // LIST_MODEL_HPP
