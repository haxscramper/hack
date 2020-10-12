#ifndef FUZZY_MATCH_HPP
#define FUZZY_MATCH_HPP

#include <string>

namespace fts {
int get_score(const std::string& item, const std::string& pattern);
}

#endif // FUZZY_MATCH_HPP
