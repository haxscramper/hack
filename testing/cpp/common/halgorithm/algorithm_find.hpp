#ifndef ALGORITHM_FIND_HPP
#define ALGORITHM_FIND_HPP

#include "algorithm_rangeoper.hpp"
#include <algorithm>
#include <functional>
#include <iterator>
#include <memory>
#include <vector>


namespace spt {
inline namespace algorithm {

    /*!
     * \brief Check if container contains target item
     */
    template <class Container>
    bool contains(
        const Container&               container,
        typename Container::value_type item) {
        return std::find(container.begin(), container.end(), item)
               != container.end();
    }

    /*!
     * \brief Check if item is contained inside brace list. Use to check if
     * item is conatined in hard-coded list of objects (ex: file extension
     * checking)
     */
    template <class Object>
    bool contains(
        typename std::initializer_list<Object> brace_list,
        Object                                 item) {
        return std::find(brace_list.begin(), brace_list.end(), item)
               != brace_list.end();
    }


    /*!
     * \brief Get index of the first element that satisfies predicate
     * filter \return Index of the item or -1 if no elements match
     * predicate
     */
    template <class Container, class Function>
    int index_if(const Container& container, Function filter) {
        auto iter = std::find_if(
            container.begin(), container.end(), filter);
        if (iter == container.end()) {
            return -1;
        } else {
            return std::distance(container.begin(), iter);
        }
    }

    /*!
     * \brief Get index of the item in container
     * \return Index of the element of -1 if item has not been found
     */
    template <class Container>
    int index_of(
        const Container&               container,
        typename Container::value_type item) {
        auto iter = std::find(container.begin(), container.end(), item);
        if (iter == container.end()) {
            return -1;
        } else {
            return std::distance(container.begin(), iter);
        }
    }


    /*!
     * \return Find first item that satisfies condition and return
     * index of this item together with iterator
     *
     * \return Index and iterator to target element of -1 and
     * past-the-end if no items satisfy condition
     */
    template <class Container, class Function>
    std::pair<int, typename Container::iterator> index_iter_if(
        const Container& container,
        Function         filter) {
        auto iter = std::find(container.begin(), container.end(), filter);
        if (iter == container.end()) {
            return std::make_pair(-1, container.end());
        } else {
            return std::make_pair(
                std::distance(container.begin(), iter), iter);
        }
    }

    /*!
     * \brief Get vector of iterators that satisfy Filter
     */
    template <class Iter, class Compare, class Filter>
    Iter max_element_if_range(
        Iter    first,
        Iter    last,
        Compare comp,
        Filter  filter) {
        std::vector<Iter> filtered = filter_range(first, last, filter);
        if (filtered.size() == 1) {
            return filtered.back();
        } else if (filtered.size() == 0) {
            return last;
        }

        typename std::vector<Iter>::iterator res = std::max_element(
            filtered.begin(),
            filtered.end(),
            [&](Iter first, Iter second) {
                return comp(*first, *second);
            });

        if (res == filtered.end()) {
            return last;
        }

        return *res;
    }

    /*!
     * \brief Return iterator pointing to the max element in container.
     * Only if it satisfies filter. If container is empty return
     * past-the-end iterator.
     */
    template <class Container, class Compare, class Filter>
    typename Container::iterator max_element_if(
        Container& container,
        /// Comparison function. Returns true if first argument is less
        /// than second.
        Compare comp,
        Filter  filter) {
        auto first = container.begin();
        auto max   = container.begin();
        auto last  = container.end();
        while (first != last) {
            if (filter(*first) && comp(*max, *first)) {
                max = first;
            }
            ++first;
        }

        return max;
    }


    /*!
     * \brief Find minimal element in range that also satisifies
     * condition imposed by `filter`.
     *
     * \note Different from `std::min_element`. Can return past-the-end
     * iterator if there were no matching items
     *
     * \warning Work-in-progress
     */
    template <class Container>
    typename Container::iterator min_element_if(
        Container&      container, ///< Container to find elements in
        std::function<bool(
            typename Container::value_type&,
            typename Container::value_type&) //
                      > is_less,             ///< Compare function
        std::function<bool(typename Container::value_type&) //
                      > filter ///< Filter function
    ) {
        using Iter    = typename Container::iterator;
        Iter last     = container.end();
        Iter smallest = container.begin();
        Iter first    = smallest++;
        while (first != last) {
            if (filter(*first)) {
                if (is_less(*first, *smallest)) {
                    smallest = first;
                }
            }
            ++first;
        }

        return first;
    }


    /*!
     * \brief Find first item for which Filter returns true
     * \return Index of item and item's iterator. If no items found
     * return total number of items and iterator last
     */
    template <class Iter, class Filter>
    std::pair<uint, Iter> find_if_count(
        Iter   first,
        Iter   last,
        Filter filter) {
        uint i = 0;
        while (first != last) {
            if (filter(*first)) {
                return std::make_pair(i, first);
            }
            ++i;
            ++first;
        }

        return std::make_pair(i, last);
    }
} // namespace algorithm
} // namespace spt

#endif // ALGORITHM_FIND_HPP
