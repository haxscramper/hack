#ifndef ALGORITHM_GETTERS_HPP
#define ALGORITHM_GETTERS_HPP

#include <algorithm>
#include <functional>
#include <memory>
#include <vector>

namespace spt {
inline namespace algorithm {

    /*!
     * \brief Return base if iterator points to end of container
     */
    template <class Container>
    typename Container::value_type safe_iter_get(
        typename Container::const_iterator iterator, ///< Iterator to check
        const Container& container, ///< Container to check against
        typename Container::value_type standard ///< Default return value
    ) {
        if (iterator == container.end()) {
            return standard;
        } else {
            return *iterator;
        }
    }


    /// \todo Repalce function with std::function(typename
    /// InputContainer::value_type) ReturnType return type or something
    /// like that
    template <class ReturnType, class InputContainer>
    std::vector<ReturnType> get_from_each(
        const InputContainer& container,
        std::function<ReturnType(typename InputContainer::value_type)>
            funciton) {
        std::vector<ReturnType> result;
        for (const typename InputContainer::value_type& item : container) {
            result.push_back(funciton(item));
        }

        return result;
    }

    /*!
     * \brief Call funciton on iterator if it is valid and return value.
     * Retun base otherwise
     *
     * If Iter is pointing to the end() of Container return base, otherwise
     * return result of application of Function onto dereferenced Iter.
     */
    template <class Container, class ResultType, class Function>
    ResultType iter_call(
        typename Container::const_iterator iterator,
        Container&                         container,
        ResultType                         standard,
        Function                           function) {
        if (iterator != container.end()) {
            return function(*iterator);
        } else {
            return standard;
        }
    }

} // namespace algorithm
} // namespace spt

#endif // ALGORITHM_GETTERS_HPP
