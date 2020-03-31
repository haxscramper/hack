#include <iostream>

/*!

Create nim-like variant objects with non-discriminatory fields.

Similar to `std::variant` but allows to have named fields for types
and access them using named getter/setters.

Create enum that will be used as kind selector:

\code{.cpp}
enum Kind {
 NewColor,
 NewShape
};
\endcode

and then create struct/class:

\code{.cpp}
struct Struct {
  Kind kind;
  // Will generate private field `_shape` and create getter
  // `shape()` and setter `shape(const QSize&)`. If user tries
  // to read field with wrong shape exception will be generated.
  OF_KIND(NewColor, QColor, color);
  OF_KIND(NewShape, QSize, shape);
};
\endcode

 */
#define OF_KIND(kindVal, type, name)                                      \
  private:                                                                \
    type __##name;                                                        \
                                                                          \
  public:                                                                 \
    type name() const {                                                   \
        if (kind != kindVal) {                                            \
            throw std::logic_error(                                       \
                "Attempt to access field " #name                          \
                " which is not accessible for current kind");             \
        } else {                                                          \
            return __##name;                                              \
        }                                                                 \
    }                                                                     \
                                                                          \
    void name(const type& val) {                                          \
        kind     = kindVal;                                               \
        __##name = val;                                                   \
    }                                                                     \
                                                                          \
    auto _##name(const type& val)->typeof(*this) {                        \
        kind     = kindVal;                                               \
        __##name = val;                                                   \
        return *this;                                                     \
    }                                                                     \
                                                                          \
    auto operator=(const type& val)->typeof(*this) {                      \
        kind     = kindVal;                                               \
        __##name = val;                                                   \
        return *this;                                                     \
    }


/// Define private class member with type `MemberType`, name
/// `memberName` and getter/setters. `functionSuffix` is used for
/// making getter/setter names, and should be equal to `memberName`,
/// except for naming style.
#define DEFINE_MEMBER_GET_SET(MemberType, memberName, functionSuffix)     \
  private:                                                                \
    MemberType memberName;                                                \
                                                                          \
  public:                                                                 \
    inline void set##functionSuffix(const MemberType& value) {            \
        memberName = value;                                               \
    }                                                                     \
                                                                          \
    inline MemberType get##functionSuffix() const {                       \
        return memberName;                                                \
    }

enum Kind
{
    Kind1,
    Kind2
};

struct Struct {
    Kind kind;
    OF_KIND(Kind1, int, intField);
    OF_KIND(Kind2, float, floatField);
};

class Class
{
    DEFINE_MEMBER_GET_SET(int, intField, IntField);
};

int main() {
    Struct str;
    str = 12;
    try {
        // We are storing integer, so this should throw an error
        str.floatField();
    } catch (std::logic_error& e) {
        // Should print error
        puts(e.what());
    }

    Class cls;
    cls.getIntField();
    cls.setIntField(12);
}
