#include <iostream>

#include "debuginator_fuzzy.hpp"

#define FTS_FUZZY_MATCH_IMPLEMENTATION
#include "fts_fuzzy_match.hpp"

#include <algorithm>
#include <iostream>
#include <vector>

using score    = std::pair<int, std::string const*>;
using matchvec = std::vector<score>;
using strvec   = std::vector<std::string>;
using str      = std::string;

matchvec ftz_sort_scored(strvec& dictionary, const str& pattern) {
    matchvec matches;
    int      score;
    for (auto&& entry : dictionary)
        if (fts::fuzzy_match(pattern.c_str(), entry.c_str(), score))
            matches.emplace_back(score, &entry);

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

int main() {
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
