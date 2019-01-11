#include <vector>
#include <unordered_map>
#include <string>
#include <algorithm>
#include <functional>
#include <iostream>


/*!
 * \brief Soft container based of values from ScoreFunction.
 *
 * Complexity: \f$O(n)\f$ applications of score function. +
 * \f$O(n\log n)\f$ comparisons
 *
 * Space: \f$O(n)\f$ pairs `<Container::value_type, function retur value>
 */
template <
    class Container,    //
    class ScoreFunction //
    >
std::unordered_map<
    typename Container::value_type, //
    typename std::result_of_t<
        ScoreFunction(typename Container::value_type&)>>
    score_sort(
        Container&    container, ///< Reference to sorted container
        ScoreFunction score,     ///< Score function
        bool lowers_first = true ///< Order in which items will be sorted
    ) {

    typedef typename Container::value_type ObjectType;
    typedef typename std::result_of_t<ScoreFunction(
        typename Container::value_type&)>
        ScoreType;

    //#======    Algorithm

    std::unordered_map<ObjectType, ScoreType> scores;
    for (ObjectType& item : container) {
        scores[item] = score(item);
    }

    if (lowers_first) {
        std::sort(
            container.begin(),
            container.end(),
            [&](ObjectType& lhs, ObjectType& rhs) {
                return scores[lhs] < scores[rhs];
            });
    } else {
        std::sort(
            container.begin(),
            container.end(),
            [&](ObjectType& lhs, ObjectType& rhs) {
                return scores[lhs] > scores[rhs];
            });
    }


    return scores;
}

int main() {
    std::vector<std::string> str_vec = {"Hello", "One", "2", "sadfasd"};
    score_sort(
        str_vec,
        [](std::string& string) -> int { return string.size(); },
        false);

    for (std::string& str : str_vec) {
        std::cout << str << " " << str.size() << "\n";
    }

    return 0;
}
