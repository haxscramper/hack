#include <span>
#include <iostream>
#include <bitset>
#include <vector>
#include <optional>
#include <utility>
#include <memory>
#include <functional>

/// Helper implementation to pass multiple types around in a 'pack'
template <typename... Args>
struct arg_pack {};

using i8  = std::int8_t;
using u8  = std::uint8_t;
using i16 = std::int16_t;
using u16 = std::uint16_t;
using i32 = std::int32_t;
using u32 = std::uint32_t;
using i64 = std::int64_t;
using u64 = std::uint64_t;
// clang-format off

template <typename A, typename B> using Pair = std::tuple<A, B>;

template <typename T> using R = T&;
template <typename T> using CR   = const T&;
template <typename T> using CP   = const T*;
template <typename T> using P    = T*;
template <typename T> using C    = const T;
template <typename T> using Opt  = std::optional<T>;
template <typename T> using UPtr = std::unique_ptr<T>;
template <typename T> using SPtr = std::shared_ptr<T>;
template <typename T> using Func = std::function<T>;

template <typename A, typename B> using Pair = std::tuple<A, B>;

// clang-format on

struct finally {
    Func<void(void)> action;
    explicit finally(Func<void(void)> _action) : action(_action) {}
    ~finally() { action(); }
};

template <typename A, typename B>
struct HSlice {
    A first;
    B last;
};

template <typename T>
T succ(T);

template <typename T>
T low();

template <typename T>
T high();

template <typename T>
int ord(T val);

template <typename T>
struct Slice : public HSlice<T, T> {
    using HSlice<T, T>::first;
    using HSlice<T, T>::last;

    class iterator {
      private:
        T now;
        T last;
        // Because ranges are always inclusive but might encompass the
        // whole range of values dedicated 'pastLast' must be added to
        // avoid infinite looping on things such as `slice(char(0),
        // char(255))`
        bool pastLast;

      public:
        typedef std::forward_iterator_tag iterator_category;
        typedef T                         value_type;
        typedef T*                        pointer;
        typedef std::ptrdiff_t            difference_type;

        iterator(T value, T _last) : now(value), last(_last) {}
        iterator() : pastLast(true) {}

        T operator*() { return now; }

        iterator& operator++() {
            // If we are on the last element and are trying to move to the
            // "next" position short-circuit all edits into "on the 'past
            // last' element"
            if (now == last || pastLast) {
                pastLast = true;
            } else {
                now = succ(now);
            }
            return *this;
        }

        bool operator!=(const iterator& other) {
            if (pastLast || other.pastLast) {
                return pastLast != other.pastLast;
            } else {
                return now != other.now;
            }
        }
    };


    iterator begin() const { return iterator(first, last); }
    iterator end() const { return iterator(); }
};

/// Return homogeneous inclusive slice of values
template <typename T>
Slice<T> slice(CR<T> first, CR<T> last) {
    return {first, last};
}

/// Return heterogeneous inclusive slice of values
template <typename A, typename B>
HSlice<A, B> slice(CR<A> first, CR<B> last) {
    return {.first = first, .last = last};
}

struct BackwardsIndex {
    int value;
};

BackwardsIndex backIndex(int value) {
    return BackwardsIndex{.value = value};
}

BackwardsIndex operator"" _B(unsigned long long int value) {
    return backIndex(value);
}

template <typename Container, typename A, typename B>
Pair<A, A> getSpan(
    Container    container,
    HSlice<A, B> s,
    bool         checkRange = true) {
    const A    startPos = s.first;
    A          endPos;
    const auto size = container.size();
    if constexpr (std::is_same_v<B, BackwardsIndex>) {
        endPos = size - s.last.value;
    } else {
        endPos = s.last.value;
    }

    if (checkRange && size <= startPos) {
        throw std::out_of_range(
            "Vector start index is out of range: span start is "
            + std::to_string(startPos)
            + ", but full vector length is only " + std::to_string(size));
    }


    if (checkRange && size <= endPos) {
        throw std::out_of_range(
            "Vector end index is out of range: span end is "
            + std::to_string(startPos)
            + ", but full vector length is only " + std::to_string(size));
    }

    return {startPos, endPos};
}

template <typename T>
class Vec : public std::vector<T> {
  public:
    using std::vector<T>::vector; // Inherit constructor from std::vector
    using std::vector<T>::size;
    using std::vector<T>::at;
    using std::vector<T>::operator[];

    operator R<std::vector<T>>() {
        return static_cast<std::vector<T>>(*this);
    }

    operator CR<std::vector<T>>() const {
        return static_cast<std::vector<T>>(*this);
    }


    template <typename A, typename B>
    std::span<T> at(CR<HSlice<A, B>> s, bool checkRange = true) {
        const auto [start, end] = getSpan(*this, s, checkRange);
        return std::span(this->data() + start, end);
    }

    template <typename A, typename B>
    std::span<const T> at(CR<HSlice<A, B>> s, bool checkRange = true)
        const {
        const auto [start, end] = getSpan(*this, s, checkRange);
        return std::span(this->data() + start, end);
    }

    template <typename A, typename B>
    std::span<T> operator[](CR<HSlice<A, B>> s) {
        return at(s, false);
    }

    template <typename A, typename B>
    std::span<const T> operator[](CR<HSlice<A, B>> s) {
        return at(s, false);
    }
};


struct Str : public std::string {
    using std::string::string;
    bool startsWith(const std::string& prefix) {
        return find(prefix) == 0;
    }

    bool endsWith(const std::string& suffix) {
        return rfind(suffix) == length() - suffix.length();
    }

    bool contains(const std::string& sub) {
        return find(sub) != std::string::npos;
    }

    template <typename A, typename B>
    std::string_view at(CR<HSlice<A, B>> s, bool checkRange = true) {
        const auto [start, end] = getSpan(*this, s, checkRange);
        return std::string_view(this->data() + start, end);
    }

    template <typename A, typename B>
    const std::string_view at(CR<HSlice<A, B>> s, bool checkRange = true)
        const {
        const auto [start, end] = getSpan(*this, s, checkRange);
        return std::string_view(this->data() + start, end);
    }
};

template <int N, int M>
struct pow_v {
    enum
    {
        res = N * pow_v<N, M - 1>::res
    };
};


template <int N>
struct pow_v<N, 0> {
    enum
    {
        res = 1
    };
};

template <>
int succ(int val) {
    return val + 1;
}
template <>
char succ(char val) {
    return val + 1;
}

template <>
int ord(char c) {
    return static_cast<unsigned char>(c);
}


template <typename T>
int ord(T value) requires(std::is_enum<T>::value) {
    return static_cast<int>(value);
}

template <typename T>
T succ(T value) requires(std::is_enum<T>::value) {
    return static_cast<T>(ord(value) + 1);
}

template <typename T, typename InT>
concept ConvertibleToSet
    = (std::same_as<std::remove_cvref_t<InT>, T> //
       || std::same_as<std::remove_cvref_t<InT>, Slice<T>>);

template <typename T, typename... U>
concept AllConvertibleToSet = (ConvertibleToSet<T, U> && ...);


/// \brief Packet set of integral values
template <typename T>
requires(sizeof(T) <= sizeof(unsigned short)) struct IntSet {
    // constrain the size of the object to avoid blowing up the set size.
    // 2-byte value has 8192 possible states and they all must be encoded
    // into the bitset, creating an 8kb object. 3 bytes will have a size of
    // 16777216 and I've decided it is a bit over the top for bitset. Maybe
    // in the future I will dispatch into different implementation bases
    // depending on the size/type of the value, but for now this check is
    // added purely for footgun reasons.
  private:
    static inline std::size_t toIdx(CR<T> value) { return ord(value); }

  public:
    using BitsetT = std::bitset<pow_v<2, 8 * sizeof(T)>::res>;
    BitsetT values;

    bool contains(CR<T> value) const { return values.test(toIdx(value)); }

    void incl(CR<IntSet<T>> other) { values |= other.values; }
    void excl(CR<IntSet<T>> other) { values &= ~other.values; }
    void incl(CR<T> value) { values.set(toIdx(value)); }
    void excl(CR<T> value) { values.reset(toIdx(value)); }
    void incl(CR<Slice<T>> range) {
        for (const auto val : range) {
            incl(val);
        }
    }

    void excl(CR<Slice<T>> range) {
        for (const auto val : range) {
            excl(val);
        }
    }


    IntSet<T> operator-(CR<IntSet<T>> other) const {
        IntSet<T> result = *this;
        result.excl(other);
        return result;
    }

    IntSet<T> operator+(CR<IntSet<T>> other) const {
        IntSet<T> result = *this;
        result.incl(other);
        return result;
    }

    int size() const { return values.count(); }


    void buildSet(R<IntSet<T>> result) {}


    template <typename ValueT>
    void buildSet(R<IntSet<T>> result, CR<ValueT> value) requires
        ConvertibleToSet<T, ValueT> {
        result.incl(value);
    }

    template <typename Value, typename... Args>
    void buildSet(
        R<IntSet<T>> result,
        CR<Value>    value,
        Args&&... tail) requires
        AllConvertibleToSet<T, Args...> && ConvertibleToSet<T, Value> {
        result.incl(value);
        buildSet(result, tail...);
    }

    /// Variadic constructor for the set type. It accepts only types that
    /// can be directly converted to the set
    template <typename... Args>
    IntSet(Args&&... args) requires AllConvertibleToSet<T, Args...> {
        buildSet(*this, args...);
    }

    class iterator {
      private:
        std::size_t index;
        BitsetT*    base;

      public:
        typedef std::forward_iterator_tag iterator_category;
        typedef T                         value_type;
        typedef T*                        pointer;
        typedef T&                        reference;
        typedef std::ptrdiff_t            difference_type;

        iterator(std::size_t _index, BitsetT* _base)
            : index(_index), base(_base) {}

        T operator*() { return static_cast<T>(index); }

        iterator& operator++() {
            // If current value is ok, step over it once
            if (index < base->size() && base->test(index)) {
                ++index;
            }
            // Otherwise step over all empty values
            while (index < base->size() && !base->test(index)) {
                ++index;
            }
            return *this;
        }

        bool operator!=(const iterator& other) {
            return index != other.index;
        }
    };

    iterator begin() { return iterator(0, &values); }
    iterator end() { return iterator(values.size(), &values); }
};

enum class SomeEnum : unsigned short
{
    FirstValue    = 0,
    SecondValue   = 1,
    RangeStart    = 2,
    RangeElement1 = 3,
    RangeElement2 = 4,
    RangeEnd      = 5
};

struct PosStr {
    Str* base;
    int  pos;
    int  line;
    int  column;
};

struct ParseError : public std::exception {};
struct LexerError : public ParseError {};
struct UnexpectedCharError : public LexerError {};
struct UnbalancedWrapError : public LexerError {};
struct MalformedTokenError : public LexerError {};


namespace charsets {
/// All character values
C<IntSet<char>> AllChars{slice('\x00', '\xFF')};
/// Arabic digits
C<IntSet<char>> Digits{slice('0', '9')};
/// Characters that can be used in C++-style identifiers
C<IntSet<char>> IdentChars{
    slice('a', 'z'),
    slice('A', 'Z'),
    slice('0', '9'),
    '_'};

/// Characters that can be used as an identifier start
C<IntSet<char>> IdentStartChars{slice('a', 'z'), slice('A', 'Z'), '_'};
/// Lowercase and uppercase latin letters
C<IntSet<char>> Letters{slice('A', 'Z'), slice('a', 'z')};
C<IntSet<char>> Newlines{'\r', '\n'};
/// Any kind of horizontal or vertical whitespace
C<IntSet<char>> Whitespace{' ', '\t', '\v', '\r', '\n', '\f'};

/// Any character that can be a part of UTF-8 encoded string
C<IntSet<char>> Utf8Any{slice('\x80', '\xFF')};
/// UTF8 continuation
C<IntSet<char>> Utf8Continuations{
    slice(char(0b10000000), char(0b10111111))};
/// Start of the two-byte utf8 rune
C<IntSet<char>> Utf8Starts2{slice(char(0b11000000), char(0b11011111))};
/// Start of the three-byte utf8 rune
C<IntSet<char>> Utf8Starts3{slice(char(0b11100000), char(0b11101111))};
/// Start of the four-byte utf8 rune
C<IntSet<char>> Utf8Starts4{slice(char(0b11110000), char(0b11110111))};
/// Start of any utf8 rune
C<IntSet<char>> Utf8Starts = Utf8Starts2 + Utf8Starts3 + Utf8Starts4;

C<IntSet<char>> LowerAsciiLetters{slice('a', 'z')};
C<IntSet<char>> HighAsciiLetters{slice('A', 'Z')};
C<IntSet<char>> AsciiLetters    = LowerAsciiLetters + HighAsciiLetters;
C<IntSet<char>> AnyRegularAscii = {slice('\x00', '\x7F')};
C<IntSet<char>> ControlChars    = {slice('\x00', '\x1F'), '\x7F'};
C<IntSet<char>> MaybeLetters    = AsciiLetters + Utf8Any;
C<IntSet<char>> IntegerStartChars{slice('0', '9'), '-', '+'};
C<IntSet<char>> HexDigitsLow = IntSet<char>{'a', 'b', 'c', 'd', 'e', 'f'}
                             + Digits;
C<IntSet<char>> HexDigitsHigh = IntSet<char>{'A', 'B', 'C', 'D', 'E', 'F'}
                              + Digits;
C<IntSet<char>> HexDigits = HexDigitsLow + HexDigitsHigh;
C<IntSet<char>> PunctOpenChars{'(', '[', '{', '<'};
C<IntSet<char>> PunctCloseChars{')', ']', '}', '>'};
C<IntSet<char>> PunctSentenceChars{',', '.', '?', '!', ';', ':'};
C<IntSet<char>> MathChars  = {'+', '/', '%', '*', '='};
C<IntSet<char>> PunctChars = PunctOpenChars + PunctCloseChars
                           + PunctSentenceChars;
C<IntSet<char>> Newline{'\n'};
C<IntSet<char>> AllSpace        = Whitespace;
C<IntSet<char>> HorizontalSpace = AllSpace - Newline;
C<IntSet<char>> DashIdentChars  = LowerAsciiLetters + HighAsciiLetters
                               + IntSet<char>{'_', '-'};
C<IntSet<char>> VeritcalSpace = Newline;

// Character found in regular text line. All chars excluding special
// controls (newline, line feed, carriage return etc.). This does include
// tabulation, because it is not uncommon in regular text.
C<IntSet<char>> TextLineChars = AllChars - ControlChars
                              + IntSet<char>{'\t'};
} // namespace charsets

int main() {
    std::cout << sizeof(SomeEnum) << " " << ord('\0') << ' ' << ord('\xFF')
              << std::endl;
    {
        IntSet<SomeEnum> s{
            SomeEnum::FirstValue,
            slice(SomeEnum::RangeStart, SomeEnum::RangeEnd)};

        std::cout << "set size: " << sizeof(s) << std::endl;
        s.contains(SomeEnum::FirstValue);
        for (const auto v : s) {
            std::cout << "--: " << static_cast<int>(v) << std::endl;
        }
    }
    const Vec<int> v{1, 2, 3, 4, 5};
    for (int x : v.at(slice(1, 2_B))) {
        std::cout << x << std::endl;
    }

    Str              str  = "random test string";
    std::string_view view = str.at(slice(1, 2_B));
    for (const auto c : view) {
        std::cout << "[" << c << "]\n";
    }
    std::cout << view.size() << "\n";
}
