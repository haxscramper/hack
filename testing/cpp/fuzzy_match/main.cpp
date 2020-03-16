#include <iostream>

#include <QAbstractListModel>
#include <QApplication>
#include <QDebug>
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
#include <iostream>
#include <unordered_map>
#include <vector>

#define let const auto
#define var auto

using score    = std::pair<int, std::string const*>;
using matchvec = std::vector<score>;
using strvec   = std::vector<std::string>;
using str      = std::string;

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
    strvec dictionary = {"44 TT", "aaa", "tt 44", "aatt 44"};
    strvec patterns   = {"tt", "tt 44", "44 tt"};
    for (const auto& patt : patterns) {
        ftz_print_matches(dictionary, patt);
    }
    // ftz_print_matches(dictionary, "tt 44");
    // ftz_print_matches(dictionary, "44 tt");

    for (const auto& patt : patterns) {
        std::cout << "pattern: " << patt << "\n";
        for (const auto& dict : dictionary) {
            std::cout << get_score(dict.data(), patt.data()) << " " << dict
                      << "\n";
        }
    }

    return 0;
}

class ListItemModel : public QAbstractListModel
{
  public:
    strvec items;

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
            //            qDebug() << "Return base data for row" <<
            //            index.row();
            return index.row();
        } else {
            return QVariant();
        }
    }

    const std::string& strAt(const int& idx) const {
        return items.at(idx);
    }
};

class SearchProxyModel : public QSortFilterProxyModel
{
    /// Index in source model (row) mapped to score
    std::unordered_map<int, int> scores;
    ListItemModel* list; /// Pointer to original source model. Stored as
                         /// member to avoid expensive dynamic cast on each
                         /// `data()` call

  public:
    /// Update item scores for source model using new `pattern`.
    void update_scores(std::string pattern) {
        if (sourceModel() == nullptr) {
            throw std::logic_error(
                "Attempt to call `update_scores` without source model - "
                "call `setSourceModel` with approproate value to fix this "
                "error");
        }

        scores.clear();
        //-- Source model
        list = dynamic_cast<ListItemModel*>(sourceModel());

        if (list == nullptr) {
            throw std::logic_error(
                "Attempt to use item model that is not `ListItemMode` - "
                "use only classes derived from `ListItemModel` when "
                "running `setSourceModel()`");
        }


        //-- Number of items to sort
        let itemcount = sourceModel()->rowCount();
        //-- iterate over all items and assign new scores to them, store
        //-- scores in `scores` map for later use
        for (int i = 0; i < itemcount; ++i) {
            let idx = qvariant_cast<int>(
                sourceModel()->data(sourceModel()->index(i, 0)));
            let data = list->strAt(idx);
            int score;
            fts::fuzzy_match(pattern.c_str(), data.c_str(), score);
            scores[idx] = score;
        }
    }


    // QSortFilterProxyModel interface
  protected:
    bool lessThan(
        const QModelIndex& source_left,
        const QModelIndex& source_right) const override {
        int lhs_row = qvariant_cast<int>(sourceModel()->data(source_left));
        int rhs_row = qvariant_cast<int>(
            sourceModel()->data(source_right));

        return scores.at(lhs_row) < scores.at(rhs_row);
    }

    // QAbstractItemModel interface
  public:
    QVariant data(const QModelIndex& index, int role) const override {
        if (role != Qt::DisplayRole) {
            return QVariant();
        }

        if (list == nullptr) {
            throw std::logic_error(
                "Attempt to call `data()` with `list == nullptr`. Call "
                "`update_scores` at least once to fix this error");
        }
        //        qDebug() << "Getting proxy data at " << index.row();

        let sourceIdx = mapToSource(index);

        return QString::fromStdString(list->strAt(sourceIdx.row()))
               + QString(" score is (%1)").arg(scores.at(sourceIdx.row()));
    }
};

int gui_main() {
    int          argc = 0;
    char**       argv = {};
    QApplication app(argc, argv);
    QMainWindow  window;
    auto         lyt = new QVBoxLayout();

    {

        auto input = new QLineEdit();

        input->setMaximumHeight(120);

        lyt->addWidget(input);
    }

    {
        auto view  = new QListView();
        auto proxy = new SearchProxyModel();
        auto list  = new ListItemModel();

        strvec dictionary = {"44 TT", "aaa", "tt 44", "aatt 44"};
        str    patt       = "tt";
        ftz_print_matches(dictionary, patt);

        list->items = dictionary;


        proxy->setSourceModel(list);
        view->setModel(proxy);

        proxy->update_scores(patt);

        lyt->addWidget(view);
    }

    window.setCentralWidget(new QWidget());
    window.centralWidget()->setLayout(lyt);

    window.show();
    return app.exec();
}

int main() {

    return gui_main();
}