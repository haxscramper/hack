#include "fuzzy_match.hpp"

#define FTS_FUZZY_MATCH_IMPLEMENTATION
#include "fts_fuzzy_match.hpp"

int fts::get_score(const std::string& item, const std::string& pattern) {
    int score;
    if (fts::fuzzy_match(pattern.c_str(), item.c_str(), score)) {
        return score;
    } else {
        return -1;
    }
}
