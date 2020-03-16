#ifndef SCRATCH_HPP
#define SCRATCH_HPP

#include <QDebug>
#include <QElapsedTimer>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#define FTS_FUZZY_MATCH_IMPLEMENTATION
#include "fts_fuzzy_match.hpp"


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


#endif // SCRATCH_HPP
