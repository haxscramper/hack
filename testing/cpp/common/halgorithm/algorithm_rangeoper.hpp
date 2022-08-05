#ifndef ALGORITHM_RANGEOPER_HPP
#define ALGORITHM_RANGEOPER_HPP

#include <algorithm>
#include <functional>
#include <memory>
#include <vector>

namespace spt {
inline namespace algorithm {

    /*!
     * \brief Create vector with copies of the elements that are not equal
     * (`==`) to target one
     */
    template <class Container>
    std::vector<typename Container::value_type> copy_different(
        /// Container to take elements from
        Container& container,
        /// Item to compare elements against
        const typename Container::value_type& comp) {
        std::vector<typename Container::value_type> result;
        typename Container::const_iterator first = container.begin();
        typename Container::const_iterator last  = container.end();
        while (first != last) {
            if (*first != comp) {
                result.push_back(*first);
            }
            first++;
        }
        return result;
    }


    /*!
     * \brief Copy elements of the container to std::vector
     */
    template <class Container>
    std::vector<typename Container::value_type> copy_to_vector(
        Container& container) {
        std::vector<typename Container::value_type> result;
        typename Container::iterator first = container.begin();
        typename Container::iterator last  = container.end();
        while (first != last) {
            result.push_back(*first);
            first++;
        }
        return result;
    }

    /*!
     * \brief Move all obkects from old container to_insert to target
     * container
     */
    template <
        class TargetContainer,
        class SourceCountainer>
    void concatenate_move(
        TargetContainer&  target,   ///< Container to append value into
        SourceCountainer& to_insert ///< Container to move values from
    ) {
        target.insert(
            target.end(),
            std::make_move_iterator(to_insert.begin()),
            std::make_move_iterator(to_insert.end()));
    }


    /*!
     * \brief Version of concatenate_move that works with rvalue references
     */
    template <class TargetContainer, class SourceCountainer>
    void concatenate_move(
        TargetContainer&   target,
        SourceCountainer&& to_insert) {
        SourceCountainer temp = std::move(to_insert);
        target.insert(
            target.end(),
            std::make_move_iterator(temp.begin()),
            std::make_move_iterator(temp.end()));
    }


    /*!
     * \brief Concatenate two containers by copying content of to_insert
     * container into the target container
     */
    template <
        class TargetContainer,
        class SourceCountainer,
        typename std::enable_if<                      //
            std::is_copy_assignable<                  //
                typename SourceCountainer::value_type //
                >                                     //
            ::value>                                  //
        ::type* = nullptr>
    void concatenate_copy(
        TargetContainer&        target,   ///< Container to insert into
        const SourceCountainer& to_insert ///< Container to copy from
    ) {
        target.insert(target.end(), to_insert.begin(), to_insert.end());
    }


    /*!
     * \brief Return true if give container's `begin()` is equal to
     * item
     */
    template <class Container>
    bool starts_with(
        Container&                     container,
        typename Container::value_type item) {
        if (container.begin() == container.end()) {
            return false;
        } else {
            return *container.begin() == item;
        }
    }


    /*!
     * \brief Check of first container completely equal to the second one
     * for at least the length of second one
     */
    template <class Container>
    bool starts_with(
        Container& container, ///< Target container to check
        Container  check      ///< Pattern container to use for checking
    ) {
        if (container.begin() == container.end()
            || check.begin() == check.end()) {

            return (
                container.begin() == container.end()
                && check.begin() == check.end());
        } else {
            using Iter           = typename Container::iterator;
            Iter container_first = container.begin();
            Iter container_end   = container.end();
            Iter check_first     = check.begin();
            Iter check_end       = check.end();

            while (check_first != check_end
                   && container_first != container_end) {
                if (*container_first == *check_first) {
                    ++container_first;
                    ++check_first;
                } else {
                    return false;
                }
            }

            if (check_first == check_end) {
                return true;
            } else {
                return false;
            }
        }
    }

    /*!
     * \brief Transform values from first container and insert results into
     * target container
     */
    template <class SourceContainer, class TargetContainer>
    void transform_insert_back(
        /// Source container to transform values from
        SourceContainer& source,
        /// Target container to insert values into
        TargetContainer& target,
        /// Transformation function
        std::function<typename TargetContainer::value_type(
            typename SourceContainer::value_type&)> converter) {
        std::transform(
            source.begin(),
            source.end(),
            std::back_inserter(target),
            converter);
    }
} // namespace algorithm
} // namespace spt

#endif // ALGORITHM_RANGEOPER_HPP
