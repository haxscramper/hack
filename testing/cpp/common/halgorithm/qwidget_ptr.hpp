#ifndef QWIDGET_PTR_HPP
#define QWIDGET_PTR_HPP

#include <QObject>

namespace spt {
inline namespace wrappers {
    /*!
     * \brief Thin wrapper around raw pointer to use in Qt parent-child
     * system for widgets
     *
     * Class provides very thin wrapper around raw pointer and inteded to
     * be used to explicitly indicate that object will be destroyed
     * together with parent object. This class does **not** keep track of
     * object count/uniqueness or anything like that.
     *
     * \note
     * Class provides tools for setting/copying and resetting pointer. If
     * you want more rigid construction see \ref qwidget_cptr
     */
    template <typename T>
    class qwidget_ptr
    {
      public:
        qwidget_ptr(T* const _ptr ///< Pointer to object. Must be
                                  ///< derivative of QObject
                    )
            : ptr(_ptr) {
            static_assert(
                std::is_convertible<T*, QObject*>::value,
                "Class must be a derivative of QWidget");

            // TODO Static check nullptr
        }

        /// \brief STL-like pointer access
        T* get() const noexcept {
            return ptr;
        }

        /// \brief Set value of pointer
        void set(T* const _ptr) {
            ptr = _ptr;
        }


        /// \brief Return value of pointer and set it to nullptr
        T* release() {
            T* _ptr = ptr;
            ptr     = nullptr;
            return _ptr;
        }

        /// \brief Return value of old and set member pointer to _ptr value
        T* reset(T* _ptr) {
            T* __ptr = ptr;
            ptr      = _ptr;
            return __ptr;
        }

        /// \brief Dereference operator
        T& operator*() {
            return *ptr;
        }

        /// \brief Class member access operator
        T* operator->() const noexcept {
            return ptr;
        }

        /// \brief Implicit conversion to unerlying pointer type
        operator T*() const noexcept {
            return ptr;
        }

        /// \brief Convinience function to perform qobject_cast
        template <class Other>
        Other* cast_to() const {
            return qobject_cast<Other*>(this);
        }

        /// \brief Convinience function to check if pointer is null
        bool is_null() const noexcept {
            return ptr == nullptr;
        }

        /// \brief Convinience function for settings parent widgets
        void set_parent(const QWidget* const widget) {
            ptr->setParent(widget);
        }

        /// \brief Convinience function for checking if object can be
        /// casted Target class \tparam Target target class. Must be
        /// derivative of QObject
        template <class Target>
        bool is_a() const {
            return qobject_cast<Target*>(ptr) != nullptr;
        }

      private:
        T* const ptr;
    };

} // namespace wrappers
} // namespace spt
#endif // QWIDGET_PTR_HPP
