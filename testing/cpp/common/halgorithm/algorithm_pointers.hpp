#ifndef ALGORITHM_POINTERS_HPP
#define ALGORITHM_POINTERS_HPP

#include <algorithm>
#include <memory>
#include <vector>

#include "algorithm_getters.hpp"

namespace spt {
inline namespace algorithm {
    /*!
     * \brief Return raw pointer that satisifes condition
     */
    template <class T, class Function>
    T* safe_uptr_get(
        const std::vector<std::unique_ptr<T>>& container, ///< Container to
                                                          ///< get raw
                                                          ///< pointer from
        Function function                                 ///< Condition
    ) {
        auto iter = std::find_if(
            container.begin(), container.end(), function);
        if (iter == container.end()) {
            return nullptr;
        } else {
            return (*iter).get();
        }
    }


    /*!
     * \brief Find unique_ptr that manages target raw pointer, release raw
     * pointer from vector and then delete unique pointer from vector
     */
    template <class T>
    std::unique_ptr<T> release_uptr(
        std::vector<std::unique_ptr<T>>& vector,
        T*                               target) {
        auto iter = std::find_if(
            vector.begin(),
            vector.end(),
            [&](const std::unique_ptr<T>& uptr) {
                return uptr.get() == target;
            });

        std::unique_ptr<T> uptr = std::unique_ptr<T>((*iter).release());
        vector.erase(iter);
        return std::move(uptr);
    }


    /*!
     * \brief Get raw pointers from each of the unique pointer from vector
     * of unique pointers
     */
    template <class T>
    const std::vector<T*> get_raw_ptrs(
        const std::vector<std::unique_ptr<T>>& input) {
        std::vector<T*> result;
        for (const std::unique_ptr<T>& uptr : input) {
            result.push_back(uptr.get());
        }
        return result;
    }

    /*!
     * \brief Get raw pointers from each of the unique pointer from vector
     * of unique pointers
     */
    template <class T>
    std::vector<T*> get_raw_ptrs(std::vector<std::unique_ptr<T>>& input) {
        std::vector<T*> result;
        for (std::unique_ptr<T>& uptr : input) {
            result.push_back(uptr.get());
        }
        return result;
    }
} // namespace algorithm
} // namespace spt


#endif // ALGORITHM_POINTERS_HPP
