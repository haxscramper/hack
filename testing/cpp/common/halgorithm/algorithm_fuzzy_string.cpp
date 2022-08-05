#include "algorithm_fuzzy_string.hpp"

namespace spt {
int generic_string_score(
    std::string&                        source,  ///< Source string
    std::string&                        pattern, ///< Pattern string
    std::vector<std::string::iterator>& matches,
    params::StringComparson             params) {
    //#======    Algorithm
    int score = 100;


    { //#======    Penalties

        // How far was first character that matched something in source
        int source_distance = std::distance(
            matches.front(), source.begin());
        score += params.leading_letter_penalty * source_distance;

        // Reward matching of first characters
        score += source_distance <= 1 ? params.first_letter_bonus : 0;
    }


    //#======    Return
    return score;
}
} // namespace spt
