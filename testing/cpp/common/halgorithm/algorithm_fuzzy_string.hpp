#ifndef ALGORITHM_FUZZY_STRING_HPP
#define ALGORITHM_FUZZY_STRING_HPP

#include <tuple>
#include <vector>

#define MAX_RECURSION_DEPTH 10

namespace spt {
inline namespace algorithm {
    template <
        class Container,
        class ScoreFunction,
        class ScoreFunctionParams>
    /// \todo Refactor to accept less arguments. Now this is a complete
    /// monster
    bool fuzzy_match_recurse(
        Container&                                 source,
        Container&                                 pattern,
        int&                                       score,
        typename Container::iterator               source_pos,
        typename Container::iterator               pattern_pos,
        std::vector<typename Container::iterator>& matches,
        ScoreFunction                              scoreFunction,
        ScoreFunctionParams                        score_parameters) {

        //#======    Recursion
        // control
        typedef typename Container::iterator Iter;
        static int                           recursion_level = 0;
        recursion_level++;

        if (recursion_level > MAX_RECURSION_DEPTH) {
            return false;
        }

        if (source_pos == source.end() || pattern_pos == pattern.end()) {
            return false;
        }

        //#======    Algorithm

        bool              recursive_match = false;
        std::vector<Iter> best_recursive_matches;
        int               best_recursive_score = 0;


        while (pattern_pos != pattern.end()
               && source_pos != source.end()) {
            if (*pattern_pos == *source_pos) { // Two objects match
                int               new_recurisve_score;
                std::vector<Iter> recursive_matches;
                if (fuzzy_match_recurse(
                        source,
                        pattern,
                        new_recurisve_score,
                        std::next(source_pos),
                        pattern_pos,
                        recursive_matches,
                        scoreFunction,
                        score_parameters) //
                ) {
                    if (!recursive_match
                        || new_recurisve_score > best_recursive_score) {
                        best_recursive_matches = recursive_matches;
                        best_recursive_score   = new_recurisve_score;
                    }
                    recursive_match = true;
                }

                matches.push_back(source_pos);
                ++pattern_pos;
            }
            ++source_pos;
        }

        bool matched = pattern_pos == pattern.end();

        if (matched) {
            score = scoreFunction(
                source, pattern, matches, score_parameters);
        }

        //#======    Return
        recursion_level--;
        if (recursive_match
            && (!matched || best_recursive_score > score)) {
            matches = best_recursive_matches;
            score   = best_recursive_score;
            return true;
        } else if (matched) {
            return true;
        } else {
            if (matches.size() != 0) {
                score = scoreFunction(
                    source, pattern, matches, score_parameters);
            } else {
            }
            return false;
        }
    }

    template <
        class Container,
        class ScoreFunction,
        class ScoreFunctionParams>
    std::tuple<bool, int, std::vector<typename Container::iterator>> fuzzy_match(
        Container&          source,
        Container&          pattern,
        ScoreFunction       scoreFunction,
        ScoreFunctionParams parameters) {
        std::vector<typename Container::iterator> matches;
        int                                       score = 0;

        bool matched = fuzzy_match_recurse(
            source,
            pattern,
            score,
            source.begin(),
            pattern.begin(),
            matches,
            scoreFunction,
            parameters);

        return std::make_tuple(matched, score, matches);
    }

    namespace params {
        struct StringComparson {
            int first_letter_bonus     = 15;
            int leading_letter_penalty = -5;
        };
    } // namespace params

    int generic_string_score(
        std::string&                        source,
        std::string&                        pattern,
        std::vector<std::string::iterator>& matches,
        params::StringComparson             params);

} // namespace algorithm
} // namespace spt

#endif // ALGORITHM_FUZZY_STRING_HPP
