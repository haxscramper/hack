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

matchvec ftz_sort_scored(strvec& dictionary, const str& pattern) {
    matchvec matches;
    int      score;
    for (auto&& entry : dictionary) {
        if (fts::fuzzy_match(pattern.c_str(), entry.c_str(), score)) {
            matches.emplace_back(score, &entry);
        }
    }

    std::sort(matches.begin(), matches.end(), [](auto&& a, auto&& b) {
        return a.first > b.first;
    });

    return matches;
}

void ftz_print_matches(strvec& dictionary, const str& pattern) {
    std::cout << "matching against '" << pattern << "'\n";
    for (const score& res : ftz_sort_scored(dictionary, pattern)) {
        std::cout << res.first << "  " << *res.second << "\n";
    }
}

int cli_main() {
    //#define use_pairs

#ifdef use_pairs
    std::vector<std::pair<const std::string, int>> items;
#else
    std::vector<std::string> items;
#endif


    std::ifstream infile("/tmp/thefile.txt");
    str           buf;
    while (std::getline(infile, buf)) {
#ifdef use_pairs
        items.push_back({buf, -1});
#else
        items.push_back(buf);
#endif
    }


    qDebug() << "Running on " << items.size() << " items";

    QElapsedTimer timer;
    timer.start();
    strvec patterns = {"QtCreator", "Nim", "tt"};
    for (let patt : patterns) {
        std::cout << "Pattern is " << patt << std::endl;
        for (auto& item : items) {
#ifdef use_pairs
            fts::fuzzy_match(
                patt.c_str(), item.first.c_str(), item.second);
            std::cout << "(" << item.second << ") " << item.first
                      << std::endl;
#else
            int score;
            fts::fuzzy_match(patt.c_str(), item.c_str(), score);
            std::cout << "(" << item << ") " << score << std::endl;
#endif
        }
    }

    qDebug() << "test completed in" << timer.nsecsElapsed() / 1000000
             << "msec";

    //    // ftz_print_matches(dictionary, "tt 44");
    //    // ftz_print_matches(dictionary, "44 tt");

    //    for (const auto& patt : patterns) {
    //        std::cout << "pattern: " << patt << "\n";
    //        for (const auto& dict : dictionary) {
    //            std::cout << get_score(dict.data(), patt.data()) << " "
    //            << dict
    //                      << "\n";
    //        }
    //    }

    return 0;
}


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

    void print_items() {
        for (let it : items) {
            std::cout << "(" << it.second << ") "
                      << "[" << it.first << "] \n";
        }
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

    // QAbstractItemModel interface
  public:
    int rowCount(const QModelIndex& parent
                 [[maybe_unused]]) const override {
        return items.size();
    }

    QVariant data(const QModelIndex& index, int role [[maybe_unused]])
        const override {
        //        return items[index.row()];
        if (role == Qt::DisplayRole) {
            // It is necessary to return data only for display role for it
            // to be displayed correctly
            return QString::fromStdString(items[index.row()].first);
        } else {
            return QVariant();
        }
    }
};

class SearchProxyModel : public QSortFilterProxyModel
{
    /// Index in source model (row) mapped to score
    score_vec_t*   scores;
    ListItemModel* list; /// Pointer to original source model. Stored as
                         /// member to avoid expensive dynamic cast on each
                         /// `data()` call. Value of this variable is used
                         /// to determine whether or not `update_scores`
                         /// has been called succesfully earlier.
    bool initDone = false;

  public:
    /// Update item scores for source model using new `pattern`.
    void update_scores(std::string pattern) {
        if (sourceModel() == nullptr) {
            throw std::logic_error(
                "Attempt to call `update_scores` without source model - "
                "call `setSourceModel` with approproate value to fix this "
                "error");
        }

        list = dynamic_cast<ListItemModel*>(sourceModel());

        if (list == nullptr) {
            throw std::logic_error(
                "Attempt to use item model that is not `ListItemMode` - "
                "use only classes derived from `ListItemModel` when "
                "running `setSourceModel()`");
        }

        scores = list->get_scores();
        list->update_scores(pattern);
        initDone = true;
    }


    // QSortFilterProxyModel interface
  protected:
    bool lessThan(
        const QModelIndex& source_left,
        const QModelIndex& source_right) const override {
        int lhs_row = qvariant_cast<int>(sourceModel()->data(source_left));
        int rhs_row = qvariant_cast<int>(
            sourceModel()->data(source_right));

        let score_lhs = scores->at(lhs_row).second;
        let score_rhs = scores->at(rhs_row).second;

        std::cout << QString("[%1] = %2 > [%3] = %4 : %5")
                         .arg(lhs_row)
                         .arg(score_lhs)
                         .arg(rhs_row)
                         .arg(score_rhs)
                         .arg(score_lhs > score_rhs)
                         .toStdString()
                  << "\n";

        return score_lhs > score_rhs;
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

    // QAbstractItemModel interface
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
