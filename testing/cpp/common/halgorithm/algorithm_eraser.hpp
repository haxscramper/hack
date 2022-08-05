#ifndef ALGORITHM_ERASER_HPP
#define ALGORITHM_ERASER_HPP

#include <algorithm>
#include <memory>
#include <vector>

namespace spt {
inline namespace algorithm {
    template <class Container>

    /*!
     * \brief Erase iterator from contaien only this is not past-the-end
     * iterator
     */
    void safe_erase(
        Container&                   container,
        typename Container::iterator iter) {
        if (container.end() != iter) {
            container.erase(iter);
        }
    }


    /*!
     * \brief Erase all ocurrencies of smart pointer to type T it points to
     * the same objects as item
     */
    template <typename T>
    void erase_all(
        std::vector<std::unique_ptr<T>>& container, ///< Target container
        T*                               item ///< Target object pointer
    ) {
        safe_erase(
            container,
            std::remove_if(
                container.begin(),
                container.end(),
                [&](std::unique_ptr<T>& uptr) {
                    return uptr.get() == item;
                }));
    }


    /*!
     * \brief Erase all occurencies of an item from container
     */
    template <class Container>
    void erase_all(
        Container&                     container, ///< Target container
        typename Container::value_type item       ///< Target item
    ) {
        safe_erase(
            container,
            std::remove(container.begin(), container.end(), item));
    }


    /*!
     * \brief Erase all items that match a filter
     */
    template <class Container, class Filter>
    void erase_if(Container& container, Filter filter) {
        container.erase(
            std::remove_if(container.begin(), container.end(), filter),
            container.end());
    }
} // namespace algorithm
} // namespace spt

#endif // ALGORITHM_ERASER_HPP
