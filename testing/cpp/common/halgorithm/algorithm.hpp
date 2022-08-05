#ifndef SPT_ALGORITHM_HPP
#define SPT_ALGORITHM_HPP

/*!
 * \file algorithm.hpp
 * \todo Add checks for subsitution types (is_function, is_callable etc)
 * \todo Move to main alogrithm file and replace old functions with new
 * ones
 */

#include <algorithm>
#include <functional>
#include <iterator>
#include <memory>
#include <vector>

#include "algorithm_eraser.hpp"
#include "algorithm_find.hpp"
#include "algorithm_for_each.hpp"
#include "algorithm_fuzzy_string.hpp"
#include "algorithm_getters.hpp"
#include "algorithm_pointers.hpp"
#include "algorithm_rangeoper.hpp"
#include <tuple>
#include <vector>

#define MAX_RECURSION_DEPTH 10

namespace spt {
/// Inline namespace for wrapper objects
inline namespace wrappers {} // namespace wrappers
/// Inline namespace for support template helper functions, algorithms and
/// data structures
inline namespace algorithm {
    /*!
     * \brief Swap elemets pointed by index first and second
     */
    template <class Container>
    void swap_elements(Container& container, uint first, uint second) {
        std::iter_swap(
            container.begin() + first, container.begin() + second);
    }


    /*!
     * \brief Apply Operator for first item in range for which Filter
     * returns true \return False if no items met condition and true
     * otherwise
     */
    template <class Operator, class Filter, class Iter>
    bool apply_first_if_count(
        Iter     first,
        Iter     last,
        Operator op,
        Filter   filter) {
        std::pair<uint, Iter> idx = find_if_count(first, last, filter);
        if (idx.second == last) {
            return false;
        }

        op(idx.first, *idx.second);
        return true;
    }

} // namespace algorithm
} // namespace spt

#endif // VECTOR_HPP
