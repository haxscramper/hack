#ifndef FUZZY_MATCH_HPP
#define FUZZY_MATCH_HPP

#include <QString>

namespace fts {
bool fuzzy_match(const QChar* pattern, const QChar* str, int& outScore);
bool fuzzy_match(
    const QChar* pattern,
    const QChar* str,
    int&         outScore,
    uint8_t*     matches,
    int          maxMatches);


int get_score(const QChar* item, const QChar* pattern);
} // namespace fts


#endif // FUZZY_MATCH_HPP
