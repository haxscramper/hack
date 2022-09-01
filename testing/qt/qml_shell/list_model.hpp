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

using ScoreVec = Vec<std::pair<Str, int>>;

class ModelData {
    Q_GADGET
    Q_PROPERTY(
        QString qdata READ getQdata WRITE setQdata NOTIFY qdataChanged);
    Q_PROPERTY(int score READ getScore WRITE setScore NOTIFY scoreChanged);

  public:
    QString     qdata;
    std::string original;
    int         score;
    QModelIndex proxyIndex;
    int         dataIndex;

    void    setQdata(CR<QString> _qdata) { qdata = _qdata; }
    QString getQdata() const { return qdata; }
    void    setScore(int _score) { score = _score; }
    int     getScore() const { return score; }
};

Q_DECLARE_METATYPE(ModelData);

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
    void setItems(CR<Vec<Str>> _items) {
        beginResetModel();

        items.clear();
        items.reserve(_items.size());
        for (const auto& it : _items) {
            items.push_back({it, -1});
        }

        endResetModel();
    }

    ScoreVec* getScores() { return &items; }

    void updateScores(const std::string& pattern) {
        for (size_t i = 0; i < items.size(); ++i) {
            items[i].second = fts::get_score(items[i].first, pattern);
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
            return QString::fromStdString(items[index.row()].first);
        } else {
            return QVariant();
        }
    }
};

class FuzzySearchProxyModel : public QSortFilterProxyModel {
    ScoreVec*      scores;
    ListItemModel* list;
    int            maxItemShowed = 50;
    bool           initDone      = false;

  public:
    void updateScores(std::string pattern) {
        list   = dynamic_cast<ListItemModel*>(sourceModel());
        scores = list->getScores();

        list->updateScores(pattern);
        this->invalidateFilter();

        initDone = true;
    }

    Q_INVOKABLE void sortOnPattern(CR<QString> _patt) {
        updateScores(_patt.toStdString());
        sort(0);
    }

    void setMaxItemShowed(int value);

  protected:
    bool lessThan(
        const QModelIndex& source_left,
        const QModelIndex& source_right) const override {

        return scores->at(source_left.row()).second >
               scores->at(source_right.row()).second;
    }

    inline bool filterAcceptsRow(
        int                source_row,
        const QModelIndex& source_parent [[maybe_unused]]) const override {
        if (initDone) {
            auto sc = scores->at(source_row).second;

            auto proxyRow = mapFromSource(
                                sourceModel()->index(
                                    source_row, 0, source_parent))
                                .row();

            auto res = sc > 0 && proxyRow < maxItemShowed;
            if (!res && scores->at(source_row).first.find("exec") !=
                            std::string::npos) {
                qDebug() << "discarding line with 'exec'"
                         << QString::fromStdString(
                                scores->at(source_row).first)
                         << "it has score of" << sc << "and row number is"
                         << proxyRow;
            }
            return res;
        } else {
            return source_row < maxItemShowed;
        }
    }

  public:
    QVariant data(const QModelIndex& index, int role) const override {
        if (!initDone) {
            if (role == Qt::DisplayRole && scores != nullptr) {
                return QString::fromStdString(
                    scores->at(index.row()).first);
            } else {
                return QVariant();
            }
        }
        if (role == Qt::DisplayRole) {
            if (list == nullptr) {
                throw std::logic_error(
                    "Attempt to call `data()` with `list == nullptr`. "
                    "Call "
                    "`update_scores` at least once to fix this error");
            }

            auto sourceIdx = mapToSource(index);

            QVariant result;
            auto     data   = ModelData();
            data.original   = scores->at(sourceIdx.row()).first;
            data.score      = scores->at(sourceIdx.row()).second;
            data.proxyIndex = index;
            data.dataIndex  = sourceIdx.row();
            data.qdata      = QString::fromStdString(
                scores->at(sourceIdx.row()).first);
            result.setValue(data);

            return result;
        } else if (role == Qt::FontRole) {
            QFont font;
            font.setFamily("JetBrains Mono");
            return font;
        } else if (role == Qt::DecorationRole) {
            auto sourceIdx = mapToSource(index);
            auto score     = scores->at(sourceIdx.row()).second;
            return QColor(
                255, 0, 0, score * 1.5 > 255 ? 255 : score * 1.5);
        } else {
            return QVariant();
        }
    }
};

#endif // LIST_MODEL_HPP
