#ifndef FUZZYSEARCHWIDGET_HPP
#define FUZZYSEARCHWIDGET_HPP

#include <QAbstractListModel>
#include <QLabel>
#include <QListView>
#include <QModelIndex>
#include <QPainter>
#include <QSortFilterProxyModel>
#include <QStyledItemDelegate>
#include <QWidget>

#include "common.hpp"
#include "fuzzy_match.hpp"


// TODO rename
struct ModelData {
    QString     qdata;
    std::string original;
    int         score;
    QModelIndex proxyIndex;
    int         dataIndex;
};

Q_DECLARE_METATYPE(ModelData);

class ListItemModel : public QAbstractListModel
{
    score_vec_t items;

  public:
    void setItems(const strvec& _items) {
        beginResetModel();

        items.clear();
        items.reserve(_items.size());
        for (let& it : _items) {
            items.push_back({it, -1});
        }

        endResetModel();
    }

    score_vec_t* getScores() {
        return &items;
    }

    void updateScores(const std::string& pattern) {
        for (size_t i = 0; i < items.size(); ++i) {
            items[i].second = fts::get_score(items[i].first, pattern);
        }
        //        QModelIndex index;
        //        emit        rowsInserted(index, 0, 0, QPrivateSignal());
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

class FuzzySearchProxyModel : public QSortFilterProxyModel
{
    score_vec_t*   scores;
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

    void setMaxItemShowed(int value);

  protected:
    bool lessThan(
        const QModelIndex& source_left,
        const QModelIndex& source_right) const override {

        return scores->at(source_left.row()).second
               > scores->at(source_right.row()).second;
    }

    bool filterAcceptsRow(
        int                source_row,
        const QModelIndex& source_parent [[maybe_unused]]) const override {
        if (initDone) {
            let sc = scores->at(source_row).second;

            let proxyRow = mapFromSource(sourceModel()->index(
                                             source_row, 0, source_parent))
                               .row();

            let res = sc > 0 && proxyRow < maxItemShowed;
            if (!res
                && scores->at(source_row).first.find("exec")
                       != std::string::npos) {
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

            let sourceIdx = mapToSource(index);

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
            let sourceIdx = mapToSource(index);
            let score     = scores->at(sourceIdx.row()).second;
            return QColor(
                255, 0, 0, score * 1.5 > 255 ? 255 : score * 1.5);
        } else {
            return QVariant();
        }
    }
};

class FuzzySearchWidget : public QWidget
{
    Q_OBJECT
  public:
    FuzzySearchWidget(QWidget* parent = nullptr);

    void setPattern(std::string patt);
    void setDictionary(const strvec& dict);
    void setAutoSortThreshold(int threshold = 12000);
    void setItemDelegate(QStyledItemDelegate* delegate);
    int  size() const;

    void setMaxItemShowed(int value);

  public slots:
    void setPattern(const QString& _patt);
    void sortOnPattern(const QString& _patt);

  private:
    int                    maxDictSize = 120000;
    FuzzySearchProxyModel* proxy;
    ListItemModel*         list;
    QLabel*                warnlbl;
    QListView*             view;
};

struct DraculaColors {
    static QColor background;
    static QColor currentLine;
    static QColor selection;
    static QColor foreground;
    static QColor comment;
    static QColor cyan;
    static QColor green;
    static QColor orange;
    static QColor pink;
    static QColor purple;
    static QColor red;
    static QColor yellow;
};

class ListItemDelegate : public QStyledItemDelegate
{
    Q_OBJECT

    // QAbstractItemDelegate interface
  public:
    void paint(
        QPainter*                   painter,
        const QStyleOptionViewItem& option,
        const QModelIndex&          index) const override {

        if (index.data().canConvert<ModelData>()) {
            let data = qvariant_cast<ModelData>(index.data());

            painter->fillRect(
                option.rect, QBrush(DraculaColors::background));

            painter->fillRect(
                option.rect.marginsRemoved(QMargins(1, 1, 1, 1)),
                QBrush(DraculaColors::comment));
            painter->setBrush(QBrush(DraculaColors::orange));
            painter->setFont(QFont("JetBrains Mono"));
            painter->drawText(
                option.rect.marginsRemoved(QMargins(10, 0, 0, 0)),
                Qt::AlignVCenter,
                QString("(%1) %2").arg(data.score).arg(data.qdata));
        } else {
            painter->drawText(
                option.rect,
                QString::fromStdString("ERROR converting data")
                    + index.data().typeName());
        }
    }
};

#endif // FUZZYSEARCHWIDGET_HPP
