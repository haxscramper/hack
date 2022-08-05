#ifndef QWIDGET_CPTR_HPP
#define QWIDGET_CPTR_HPP

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
     * Class holds **const** pointer and does not provide defaults
     * constructor therefore forcing to use initializer lists. If you want
     * these features you should  see \ref qobject_ptr
     */
    template <typename T>
    class qwidget_cptr
    {
      public:
        qwidget_cptr(T* const _cptr ///< Pointer to object. Must be
                                    ///< derivative of QObject
                     )
            : cptr(_cptr) {
            static_assert(
                std::is_convertible<T*, QObject*>::value,
                "Class must be a derivative of QWidget");

            // TODO Static check nullptr
        }

        qwidget_cptr(const qwidget_cptr<T>& other)  = delete;
        qwidget_cptr(const qwidget_cptr<T>&& other) = delete;
        qwidget_cptr<T>& operator=(const qwidget_cptr<T>& other) = delete;
        qwidget_cptr<T>& operator=(const qwidget_cptr<T>&& other) = delete;

        /// \brief STL-like pointer access
        T* get() const noexcept {
            return cptr;
        }

        /// \brief Dereference operator
        T& operator*() {
            return *cptr;
        }

        /// \brief Class member access operator
        T* operator->() const noexcept {
            return cptr;
        }

        /// \brief Implicit conversion to unerlying pointer type
        operator T*() const noexcept {
            return cptr;
        }

        /// \brief Convinience function to perform qobject_cast
        template <class Other>
        Other* cast_to() const {
            return qobject_cast<Other*>(cptr);
        }

        /// \brief Convinience function to check if pointer is null
        bool is_null() const noexcept {
            return cptr == nullptr;
        }

      private:
        T* const cptr;
    };
} // namespace wrappers
} // namespace spt

#endif // QWIDGET_CPTR_HPP
