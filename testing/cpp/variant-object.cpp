#include <iostream>

#define FIELD_NAME(name) __##name

#define VARIANT_FIELD_ACCESS(kindVal, type, name)                         \
  public:                                                                 \
    type name() const {                                                   \
        if (kind != kindVal) {                                            \
            throw std::logic_error(                                       \
                "Attempt to access field " #name                          \
                " which is not accessible for current kind");             \
        } else {                                                          \
            return FIELD_NAME(name);                                      \
        }                                                                 \
    }                                                                     \
                                                                          \
    void name(const type& val) {                                          \
        kind             = kindVal;                                       \
        FIELD_NAME(name) = val;                                           \
    }                                                                     \
                                                                          \
    type& _##name() {                                                     \
        if (kind != kindVal) {                                            \
            throw std::logic_error(                                       \
                "Attempt to access field " #name                          \
                " which is not accessible for current kind");             \
        } else {                                                          \
            return FIELD_NAME(name);                                      \
        }                                                                 \
    }                                                                     \
                                                                          \
    auto _##name(const type& val)->typeof(*this) {                        \
        kind             = kindVal;                                       \
        FIELD_NAME(name) = val;                                           \
        return *this;                                                     \
    }


class Test
{
  public:
    enum class Kind
    {
        Integer,
        Char,
    };

  private:
    union {
        int  FIELD_NAME(f1); // Declare union field of type `int`
        char FIELD_NAME(f2);
    };

    Kind kind;

  public:
    Kind getKind() const;

    // Create all methods for accessing fields
    VARIANT_FIELD_ACCESS(Kind::Integer, int, f1);
    VARIANT_FIELD_ACCESS(Kind::Char, char, f2);
};

int main() {
    Test t;
    t.f1(12);
    t._f1() = 24;

    try {
        std::cout << t.f2();
    } catch (std::logic_error e) { std::cout << e.what(); }
}
