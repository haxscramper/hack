#include <iostream>

#include <QAbstractListModel>
#include <QApplication>
#include <QDebug>
#include <QElapsedTimer>
#include <QLineEdit>
#include <QListView>
#include <QMainWindow>
#include <QSortFilterProxyModel>
#include <QVBoxLayout>
#include <QVariant>
#include <QWidget>
#include <QWindow>

#include "debuginator_fuzzy.hpp"

#define FTS_FUZZY_MATCH_IMPLEMENTATION
#include "fts_fuzzy_match.hpp"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <unordered_map>
#include <vector>

#define let const auto
#define var auto

using score    = std::pair<int, std::string const*>;
using matchvec = std::vector<score>;
using strvec   = std::vector<std::string>;
using str      = std::string;

using score_vec_t = std::vector<std::pair<const std::string, int>>;

class ListItemModel : public QAbstractListModel
{
    score_vec_t items;

  public:
    void set_items(const strvec& _items) {
        items.clear();
        items.reserve(_items.size());
        for (let it : _items) {
            items.push_back({it, -1});
        }
    }

    score_vec_t* get_scores() {
        return &items;
    }

    void update_scores(const std::string& pattern) {
        qDebug() << "Updating scores";
        std::cout << "Using pattern " << pattern << std::endl;
        const char* patt = pattern.c_str();
        for (size_t i = 0; i < items.size(); ++i) {
            int score;
            if (fts::fuzzy_match(patt, items[i].first.c_str(), score)) {
                items[i].second = score;
            } else {
                items[i].second = -1;
            }
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

class SearchProxyModel : public QSortFilterProxyModel
{
    score_vec_t*   scores;
    ListItemModel* list;
    bool           initDone = false;

  public:
    void update_scores(std::string pattern) {
        list = dynamic_cast<ListItemModel*>(sourceModel());

        scores = list->get_scores();
        list->update_scores(pattern);
        this->invalidateFilter();
        initDone = true;
    }


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
            return sc > 0;
        } else {
            return true;
        }
    }

  public:
    QVariant data(const QModelIndex& index, int role) const override {
        if (role == Qt::DisplayRole) {
            if (list == nullptr) {
                throw std::logic_error(
                    "Attempt to call `data()` with `list == nullptr`. "
                    "Call "
                    "`update_scores` at least once to fix this error");
            }

            let sourceIdx = mapToSource(index);

            let item = QString::fromStdString(
                scores->at(sourceIdx.row()).first);
            let score = QString("(%1) ").arg(
                scores->at(sourceIdx.row()).second);

            return score + item;
        } else if (role == Qt::FontRole) {
            QFont font;
            font.setFamily("JetBrains Mono");
            return font;
        } else {
            return QVariant();
        }
    }
};

int gui_main() {
    int          argc = 0;
    char**       argv = {};
    QApplication app(argc, argv);
    QMainWindow  window;
    auto         lyt = new QVBoxLayout();


    auto input = new QLineEdit();

    input->setMaximumHeight(120);

    lyt->addWidget(input);

    auto view  = new QListView();
    auto proxy = new SearchProxyModel();
    auto list  = new ListItemModel();

    strvec dictionary;

    std::ifstream infile("/tmp/thefile.txt");
    str           buf;
    while (std::getline(infile, buf)) {
        dictionary.push_back(buf);
    }

    qDebug() << "Filtering on " << dictionary.size() << " items\n";

    str patt = "tt";
    list->set_items(dictionary);
    proxy->setSourceModel(list);
    view->setModel(proxy);

    proxy->update_scores(patt);
    proxy->setDynamicSortFilter(false);
    proxy->sort(0);

    lyt->addWidget(view);

    QObject::connect(
        input, &QLineEdit::textChanged, [proxy](const QString& text) {
            qDebug() << "sorting ...";
            str patt = text.toStdString();
            proxy->update_scores(patt);
            proxy->sort(0);
        });

    window.setCentralWidget(new QWidget());
    window.centralWidget()->setLayout(lyt);

    window.show();
    return app.exec();
    return 0;
}

int main() {

    return gui_main();
    //    return cli_main();
}
