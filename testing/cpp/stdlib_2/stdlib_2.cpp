#include <span>
#include <variant>
#include <iostream>
#include <bitset>
#include <vector>
#include <optional>
#include <utility>
#include <memory>
#include <functional>

#include <catch2/catch_session.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <catch2/benchmark/catch_benchmark.hpp>
#include <catch2/matchers/catch_matchers_exception.hpp>

#include "enum_types.hpp"

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


i8 operator"" _i8(unsigned long long int value) {
    return static_cast<i8>(value);
}

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


template <typename A, typename B>
std::ostream& operator<<(std::ostream& os, HSlice<A, B> const& value) {
    os << "[" << value.first << ".." << value.last << "]";
    return os;
}

template <typename T>
std::ostream& operator<<(std::ostream& os, std::span<T> const& value) {
    bool first = true;
    os << "@[";
    for (const auto it : value) {
        if (!first) {
            os << ", ";
        }
        first = false;
        os << it;
    }
    os << "]";
    return os;
}

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
    bool contains(T val) { return first <= val && val <= last; }

    class iterator {
      private:
        T now;
        T last;
        // Because ranges are always inclusive but might encompass the
        // whole range of values dedicated 'pastLast' must be added to
        // avoid infinite looping on things such as `slice(char(0),
        // char(255))`
        bool pastLast = false;

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
constexpr Slice<T> slice(CR<T> first, CR<T> last) {
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
        endPos = s.last;
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

template <typename T, typename Ref>
struct EnumerateState {

    class iterator {
      private:
        T*  iter;
        int index = 0;

      public:
        typedef std::forward_iterator_tag iterator_category;

        typedef Pair<int, Ref>  value_type;
        typedef Pair<int, Ref>* pointer;
        typedef Pair<int, Ref>& reference;
        typedef std::ptrdiff_t  difference_type;

        iterator(T* it) : iter(it) {}

        Pair<int, Ref> operator*() { return {index, *(*iter)}; }

        iterator& operator++() {
            ++index;
            ++(*iter);
            return *this;
        }

        bool operator!=(const iterator& other) {
            return (*iter) != (*other.iter);
        }
    };


    iterator begin() { return iterator(&beginIterator); }
    iterator end() { return iterator(&endIterator); }

    EnumerateState(T begin, T end)
        : beginIterator(begin), endIterator(end) {}

  private:
    T beginIterator;
    T endIterator;
};

template <typename T>
EnumerateState<typename T::iterator, typename T::iterator::value_type> enumerate(
    R<T> value) {
    return EnumerateState<
        typename T::iterator,
        typename T::iterator::value_type>(value.begin(), value.end());
}


template <typename T>
EnumerateState<typename T::const_iterator, typename T::const_iterator::value_type> enumerate(
    CR<T> value) {
    return EnumerateState<
        typename T::const_iterator,
        typename T::const_iterator::value_type>(
        value.cbegin(), value.cend());
}

template <typename T>
int index_of(CR<std::vector<T>> vector, CR<T> item) {
    auto pos = std::find(vector.begin(), vector.end(), item);
    if (pos != vector.end()) {
        return std::distance(pos, vector.begin());
    } else {
        return -1;
    }
}

template <typename T>
class Vec : public std::vector<T> {
  public:
    using std::vector<T>::vector; // Inherit constructor from std::vector
    using std::vector<T>::size;
    using std::vector<T>::at;
    using std::vector<T>::operator[];
    using std::vector<T>::push_back;
    using std::vector<T>::back;
    using std::vector<T>::pop_back;
    using std::vector<T>::begin;
    using std::vector<T>::end;


    operator R<std::vector<T>>() {
        return static_cast<std::vector<T>>(*this);
    }

    operator CR<std::vector<T>>() const {
        return static_cast<std::vector<T>>(*this);
    }

    bool has(int idx) const { return idx < size(); }

    template <typename A, typename B>
    std::span<T> at(CR<HSlice<A, B>> s, bool checkRange = true) {
        const auto [start, end] = getSpan(*this, s, checkRange);
        return std::span(this->data() + start, end - start + 1);
    }

    template <typename A, typename B>
    std::span<const T> at(CR<HSlice<A, B>> s, bool checkRange = true)
        const {
        const auto [start, end] = getSpan(*this, s, checkRange);
        return std::span(this->data() + start, end - start + 1);
    }

    template <typename A, typename B>
    std::span<T> operator[](CR<HSlice<A, B>> s) {
#ifdef DEBUG
        return at(s, true);
#else
        return at(s, false);
#endif
    }

    template <typename A, typename B>
    std::span<const T> operator[](CR<HSlice<A, B>> s) const {
#ifdef DEBUG
        return at(s, true);
#else
        return at(s, false);
#endif
    }

    T& operator[](BackwardsIndex idx) {
        return (*this)[this->size() - idx.value];
    }

    T& at(BackwardsIndex idx) {
        return this->at(this->size() - idx.value);
    }

    T pop_back_v() {
        auto result = back();
        pop_back();
        return result;
    }

    int high() const { return size() - 1; }

    int indexOf(CR<T> item) const { return index_of(*this, item); }
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

template <>
int ord(i8 c) {
    return static_cast<i8>(c);
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


/*!
\brief Packet set of integral values

This type is mostly intended to be used types that have enum-like meaning
(regular enums, characters, small "state number" integers) instead of a
more general set solution.

This means that the API has been optimized for things like

```cpp
enum SomeFlags { flag1, flag2, flag3 };
IntSet<SomeFlags> flag;
flag.incl(flag1);
if (flag.contains(flag2)) {
  // do something
}
```

*/
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
    constexpr static inline std::size_t toIdx(CR<T> value) {
        return ord(value);
    }

    constexpr void buildSet(R<IntSet<T>> result) {}

    template <typename ValueT>
    constexpr void buildSet(R<IntSet<T>> result, CR<ValueT> value) requires
        ConvertibleToSet<T, ValueT> {
        result.incl(value);
    }

    // Helper method to recurse into constructor argument list
    template <typename Value, typename... Args>
    constexpr void buildSet(
        R<IntSet<T>> result,
        CR<Value>    value,
        Args&&... tail) requires
        AllConvertibleToSet<T, Args...> && ConvertibleToSet<T, Value> {
        result.incl(value);
        buildSet(result, tail...);
    }


  public:
    using BitsetT = std::bitset<pow_v<2, 8 * sizeof(T)>::res>;
    BitsetT values;

    constexpr bool contains(CR<T> value) const {
        return values.test(toIdx(value));
    }
    /// Check if one set is a proper subset of another -- \arg other
    /// contains all the values.
    constexpr bool contains(CR<IntSet<T>> other) const {
        return ((values & other.values) == other.values);
    }


    constexpr void incl(CR<IntSet<T>> other) { values |= other.values; }
    constexpr void excl(CR<IntSet<T>> other) { values &= ~other.values; }
    constexpr void incl(CR<T> value) { values.set(toIdx(value)); }
    constexpr void excl(CR<T> value) { values.reset(toIdx(value)); }
    constexpr void incl(CR<Slice<T>> range) {
        for (const auto val : range) {
            incl(val);
        }
    }

    constexpr void excl(CR<Slice<T>> range) {
        for (const auto val : range) {
            excl(val);
        }
    }


    bool operator==(CR<IntSet<T>> other) const {
        return values == other.values;
    }

    bool operator<(CR<IntSet<T>> other) const {
        return other.contains(*this) && this->size() < other.size();
    }

    bool operator<=(CR<IntSet<T>> other) const {
        return other.contains(*this);
    }

    IntSet<T> operator^(CR<IntSet<T>> other) const {
        IntSet<T> result;
        result.values = this->values ^ other.values;
        return result;
    }

    IntSet<T> operator&(CR<IntSet<T>> other) const {
        IntSet<T> result;
        result.values = this->values & other.values;
        return result;
    }

    IntSet<T> operator|(CR<IntSet<T>> other) const {
        IntSet<T> result;
        result.values = this->values | other.values;
        return result;
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

    /// Return complement of the set that contains all values that were not
    /// placed in the original set.
    IntSet<T> operator~() { return {.values = ~values}; }

    int  size() const { return values.count(); }
    bool empty() const { return size() == 0; }

    /// Variadic constructor for the set type. It accepts only types that
    /// can be directly converted to the set
    template <typename... Args>
    constexpr IntSet(Args&&... args) requires
        AllConvertibleToSet<T, Args...> {
        buildSet(*this, args...);
    }

    class iterator {
      private:
        std::size_t    index;
        BitsetT const* base;

      public:
        typedef std::forward_iterator_tag iterator_category;
        typedef T                         value_type;
        typedef T*                        pointer;
        typedef T&                        reference;
        typedef std::ptrdiff_t            difference_type;


        iterator(std::size_t _index, BitsetT const* _base)
            : index(_index), base(_base) {
            // If starting position is empty, move over to the required
            // element.
            if (index < base->size() && !base->test(index)) {
                operator++();
            }
        }

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
    iterator begin() const { return iterator(0, &values); }
    iterator end() const { return iterator(values.size(), &values); }
};

template <typename T>
std::ostream& operator<<(std::ostream& os, IntSet<T> const& value) {
    bool first = true;
    os << "{";
    for (const auto val : value) {
        if (!first) {
            os << ", ";
        }
        first = false;
        os << val;
    }
    os << "}";
    return os;
}

template <typename T>
concept StringStreamable = requires(T value, std::ostream& os) {
    { os << value } -> std::same_as<std::ostream&>;
};

template <typename T>
std::string to_string(CR<T> value) requires StringStreamable<T> {
    std::stringstream os;
    os << value;
    return os.str();
}

template <typename T>
concept StringConvertible = requires(T value) {
    { to_string(value) } -> std::same_as<std::string>;
};


void to_string_vec_impl(std::vector<std::string>& out) {}

template <typename T, typename... Tail>
void to_string_vec_impl(
    std::vector<std::string>& out,
    CR<T>&                    in,
    Tail&&... tail) requires StringConvertible<T> {
    out.push_back(to_string(in));
    to_string_vec_impl(out, tail...);
}

template <typename... Args>
std::vector<std::string> to_string_vec(Args&&... args) {
    std::vector<std::string> result{};
    result.reserve(sizeof...(Args));
    to_string_vec_impl(result, args...);
    return result;
}


using CharSet = IntSet<char>;

namespace charsets {
/// All character values
C<CharSet> AllChars{slice('\x00', '\xFF')};
/// Arabic digits
C<CharSet> Digits{slice('0', '9')};
/// Characters that can be used in C++-style identifiers
C<CharSet> IdentChars{
    slice('a', 'z'),
    slice('A', 'Z'),
    slice('0', '9'),
    '_'};

/// Characters that can be used as an identifier start
C<CharSet> IdentStartChars{slice('a', 'z'), slice('A', 'Z'), '_'};
/// Lowercase and uppercase latin letters
C<CharSet> Letters{slice('A', 'Z'), slice('a', 'z')};
C<CharSet> Newlines{'\r', '\n'};
/// Any kind of horizontal or vertical whitespace
C<CharSet> Whitespace{' ', '\t', '\v', '\r', '\n', '\f'};

/// Any character that can be a part of UTF-8 encoded string
C<CharSet> Utf8Any{slice('\x80', '\xFF')};
/// UTF8 continuation
C<CharSet> Utf8Continuations{slice(char(0b10000000), char(0b10111111))};
/// Start of the two-byte utf8 rune
C<CharSet> Utf8Starts2{slice(char(0b11000000), char(0b11011111))};
/// Start of the three-byte utf8 rune
C<CharSet> Utf8Starts3{slice(char(0b11100000), char(0b11101111))};
/// Start of the four-byte utf8 rune
C<CharSet> Utf8Starts4{slice(char(0b11110000), char(0b11110111))};
/// Start of any utf8 rune
C<CharSet> Utf8Starts = Utf8Starts2 + Utf8Starts3 + Utf8Starts4;

C<CharSet> LowerAsciiLetters{slice('a', 'z')};
C<CharSet> HighAsciiLetters{slice('A', 'Z')};
C<CharSet> AsciiLetters    = LowerAsciiLetters + HighAsciiLetters;
C<CharSet> AnyRegularAscii = {slice('\x00', '\x7F')};
C<CharSet> ControlChars    = {slice('\x00', '\x1F'), '\x7F'};
C<CharSet> MaybeLetters    = AsciiLetters + Utf8Any;
C<CharSet> IntegerStartChars{slice('0', '9'), '-', '+'};
C<CharSet> HexDigitsLow  = CharSet{'a', 'b', 'c', 'd', 'e', 'f'} + Digits;
C<CharSet> HexDigitsHigh = CharSet{'A', 'B', 'C', 'D', 'E', 'F'} + Digits;
C<CharSet> HexDigits     = HexDigitsLow + HexDigitsHigh;
C<CharSet> PunctOpenChars{'(', '[', '{', '<'};
C<CharSet> PunctCloseChars{')', ']', '}', '>'};
C<CharSet> PunctSentenceChars{',', '.', '?', '!', ';', ':'};
C<CharSet> MathChars  = {'+', '/', '%', '*', '='};
C<CharSet> PunctChars = PunctOpenChars + PunctCloseChars
                      + PunctSentenceChars;
C<CharSet> Newline{'\n'};
C<CharSet> AllSpace        = Whitespace;
C<CharSet> HorizontalSpace = AllSpace - Newline;
C<CharSet> DashIdentChars  = LowerAsciiLetters + HighAsciiLetters
                          + CharSet{'_', '-'};
C<CharSet> VeritcalSpace = Newline;

// Character found in regular text line. All chars excluding special
// controls (newline, line feed, carriage return etc.). This does include
// tabulation, because it is not uncommon in regular text.
C<CharSet> TextLineChars = AllChars - ControlChars + CharSet{'\t'};
} // namespace charsets


enum AddfFragmentKind
{
    addfText,        /// Regular text fragment
    addfPositional,  /// Positional fragment `$#`
    addfIndexed,     /// Indexed fragment `$1`
    addfDollar,      /// Dollar literal `$$`
    addfBackIndexed, /// Negative indexed fragment `$-1`
    addfVar,         /// Interpolated variable `$name`
    addfExpr,        /// Expression in braces `${some expr}`
};

struct AddfFragment {
    AddfFragmentKind kind;
    std::string      text;
    int              idx;
};

struct FormatStringError : public std::runtime_error {
    explicit FormatStringError(const std::string& message)
        : std::runtime_error(message) {}
};


/*!Iterate over interpolation fragments of the `formatstr`
 */
constexpr Vec<AddfFragment> addfFragments(const std::string& formatstr) {
    Vec<AddfFragment> result{};
    auto              i   = 0;
    auto              num = 0;
    const CharSet     PatternChars{
        slice('a', 'z'),
        slice('A', 'Z'),
        slice('0', '9'),
        slice('\xF0', '\xFF'),
        '_'};

    while (i < formatstr.size()) {
        if (((formatstr[i] == '$') && ((i + 1) < formatstr.size()))) {
            const auto c = formatstr[i + 1];
            if (c == '#') {
                result.push_back({.kind = addfIndexed, .idx = num});
                i += 2;
                num += 1;
            } else if (c == '$') {
                i += 2;
                result.push_back({.kind = addfDollar});

            } else if (charsets::Digits.contains(c) || c == '|') {
                auto j = 0;
                i += 1;
                const auto starti   = i;
                auto       negative = formatstr[i] == '-';
                if (negative) {
                    i += 1;
                }
                while (
                    ((i < formatstr.size())
                     && (charsets::Digits.contains(formatstr[i])))) {
                    j = ((j * 10) + ord(formatstr[i])) - ord('0');
                    i += 1;
                }
                if (negative) {
                    result.push_back({.kind = addfBackIndexed, .idx = j});
                } else {
                    result.push_back({.kind = addfIndexed, .idx = j - 1});
                }
            } else if (c == '{') {
                auto       j        = i + 2;
                auto       k        = 0;
                auto       negative = formatstr[j] == '-';
                const auto starti   = j;
                if (negative) {
                    j += 1;
                }
                auto isNumber = 0;
                while (
                    ((j < formatstr.size())
                     && (!IntSet<char>({'\0', '}'})
                              .contains(formatstr[j])))) {
                    if (charsets::Digits.contains(formatstr[j])) {
                        k = ((k * 10) + ord(formatstr[j])) - ord('0');
                        if (isNumber == 0) {
                            isNumber = 1;
                        }
                    } else {
                        isNumber = -1;
                    }
                    j += 1;
                };
                if (isNumber == 1) {
                    if (negative) {
                        result.push_back(
                            {.kind = addfBackIndexed, .idx = k});
                    } else {
                        result.push_back(
                            {.kind = addfIndexed, .idx = k - 1});
                    }
                } else {
                    const auto first = i + 2;
                    const auto last  = j - 1;
                    const auto count = last - first + 1;

                    result.push_back(
                        {.kind = addfExpr,
                         .text = formatstr.substr(first, count)});
                }
                i = j + 1;

            } else if (
                charsets::Letters.contains(c) || c == '_'
                || IntSet<char>(slice('\xF0', '\xFF')).contains(c)) {
                auto j = i + 1;
                while (
                    ((j < formatstr.size())
                     && (PatternChars.contains(formatstr[j])))) {
                    j += 1;
                }
                const auto first = i + 1;
                const auto last  = j - 1;

                result.push_back(
                    {.kind = addfVar,
                     .text = formatstr.substr(first, last - first + 1)});
                i = j;
            } else {
                throw FormatStringError(
                    R"(unexpected char after $ - )"
                    + std::string(1, formatstr[i + 1]));
            }
        } else {
            auto trange = slice(i, i);
            while (
                ((trange.last < formatstr.size())
                 && (formatstr[trange.last] != '$'))) {
                trange.last += 1;
            }
            trange.last -= 1;
            result.push_back(
                {.kind = addfText,
                 .text = formatstr.substr(
                     trange.first, trange.last - trange.first + 1)});
            i = trange.last;
            i += 1;
        }
    }
    return result;
}

/*! The same as `add(s, formatstr % a)`, but more efficient. */
void addf(
    std::string&                    s,
    CR<Vec<AddfFragment>>           fragments,
    const std::vector<std::string>& a) {
    for (const auto fr : fragments) {
        switch (fr.kind) {
            case addfDollar: {
                s += '$';
                break;
            }
            case addfPositional:
            case addfIndexed:
            case addfBackIndexed: {
                int idx;
                if ((fr.kind) == (addfBackIndexed)) {
                    idx = a.size() - fr.idx;
                } else {
                    idx = fr.idx;
                }
                if (idx < 0 || a.size() <= idx) {
                    throw FormatStringError(
                        "Argument index out of bounds. Accessed ["
                        + std::to_string(idx) + "], but only "
                        + std::to_string(a.size())
                        + " arguments were supplied");
                }
                s += a[idx];
                break;
            }
            case addfText: {
                s += fr.text;
                break;
            }
            case addfVar:
            case addfExpr: {
                auto x = index_of(a, fr.text);
                if ((0 <= x) && (x < (a.size() - 1))) {
                    s += a[x + 1];
                } else {
                    throw FormatStringError(
                        "No interpolation argument named '" + fr.text
                        + "'");
                };
                break;
            }
        };
    };
}


std::vector<std::string> fold_format_pairs(
    CR<std::vector<Pair<std::string, std::string>>> values) {
    std::vector<std::string> tmp;
    for (const auto& [key, val] : values) {
        tmp.push_back(key);
        tmp.push_back(val);
    }
    return tmp;
}

std::string addf(
    CR<Vec<AddfFragment>>        format,
    CR<std::vector<std::string>> values) {
    std::string result;
    addf(result, format, values);
    return result;
}

std::string operator%(
    CR<std::string>              format,
    CR<std::vector<std::string>> values) {
    return addf(addfFragments(format), values);
}

std::string operator%(
    CR<std::string>                                 format,
    CR<std::vector<Pair<std::string, std::string>>> values) {
    return addf(addfFragments(format), fold_format_pairs(values));
}


struct Str : public std::string {
    using std::string::string;
    using std::string::operator[];
    using std::string::at;

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

    template <typename A, typename B>
    std::string_view operator[](CR<HSlice<A, B>> s) {
        return at(s, false);
    }

    template <typename A, typename B>
    const std::string_view operator[](CR<HSlice<A, B>> s) const {
        return at(s, false);
    }


    Str rightAligned(int n, char c = ' ') {
        Str res;
        if (size() < n) {
            res.append(n - size(), c);
        }
        res.append(*this);
        return res;
    }

    Str leftAligned(int n, char c = ' ') {
        auto s = *this;
        if (s.size() < n) {
            s.append(n - s.size(), c);
        }
        return s;
    }
};

using StrVec     = std::vector<Str>;
using StrPairVec = Vec<Pair<Str, Str>>;


enum class SomeEnum : unsigned short
{
    FirstValue    = 0,
    SecondValue   = 1,
    RangeStart    = 2,
    RangeElement1 = 3,
    RangeElement2 = 4,
    RangeEnd      = 5
};


struct LineCol {
    int line;
    int column;
};


struct ParseError : public std::runtime_error {
    LineCol loc;
    explicit ParseError(const std::string& message, LineCol _loc)
        : std::runtime_error(message), loc(_loc) {}
};

struct LexerError : public ParseError {
    explicit LexerError(const std::string& message, LineCol _loc)
        : ParseError(message, _loc) {}
};

struct UnexpectedCharError : public LexerError {
    explicit UnexpectedCharError(const std::string& message, LineCol _loc)
        : LexerError(message, _loc) {}
};


struct UnbalancedWrapError : public LexerError {};
struct MalformedTokenError : public LexerError {};


template <typename T>
bool notNil(T* ptr) {
    return ptr != nullptr;
}

template <typename T>
bool isNil(T* ptr) {
    return ptr == nullptr;
}

// struct PosStr {
//     PosStr() {}
//     ~PosStr() {}
//     PosStr(const PosStr& other) {}
//     const std::string* baseStr; /*!For non-slice string used as buffer.
//   Might contain full input data (in case of lexing over existing
//   string). For slice string contains reference to the original string
//   (is not modified)
//   */
//     bool isSlice;
//     int  pos; /*!Current absolute position in the base/buffer string.
// Always points to the current valid character. Calling
// [[code:advance()]] changes this position, potentially by
// an unlimited amount in case of fragmented string.
// */
//     int line; /// Current line index. Automatically tracked by
//               /// [[code:advance()]]
//     int                   column; /// Current column number
//     bool                  bufferActive;
//     Vec<Vec<PosStrSlice>> sliceBuffer;
//     /// Buffer for new positional
//     /// slices. Used by [[code:startSlice()]] and [[code:finishSlice()]]
//     to
//     /// automatically collect new string slices
// };

TEST_CASE("String operations", "[str]") {
    SECTION("Basic operations") {
        Str s1{"Hello"};
        Str s2{"World"};
        Str empty;

        REQUIRE(s1.startsWith("He"));
        REQUIRE_FALSE(s1.startsWith("Wo"));
        REQUIRE(s1.endsWith("lo"));
        REQUIRE_FALSE(s1.endsWith("or"));
        REQUIRE(s1.size() == 5);
        REQUIRE_FALSE(s1.empty());
        REQUIRE(empty.empty());
        REQUIRE(s1[1] == 'e');
        // String slices are inclusive
        REQUIRE(s1[Slice<int>{1, 3}] == "ell");
        REQUIRE((s1 + s2) == "HelloWorld");
    }

    SECTION("String mutations") {
        Str s1{"01234"};
        Str s2{"World"};
        Str empty;

        // Change the first character of s1 to 'J'
        s1[0] = '!';
        REQUIRE(s1 == "!1234");
        // Try to change the first character of empty to 'X'
        REQUIRE_THROWS_AS((empty.at(0) = 'X'), std::out_of_range);
        // Try to change the last two characters of empty to "zz" using the
        // slice operator
        REQUIRE_THROWS_AS((empty.at(Slice<int>{1, 2})), std::out_of_range);
    }
}


/// Generic token containing minimal required information: span of text and
/// tag. Line/Column information can be computed on the as-needed basis
/// from the base string.
template <typename K>
struct Token {
    K                kind; /// Specific kind of the token
    std::string_view text; /// Token view on the base input text
};

struct StrPattern {
    using V = CR<std::string_view>;
    struct OneOrMore {
        SPtr<StrPattern> it;

        int matchOffset(V v) const {
            auto tmp    = v;
            int  result = it->matchOffset(v);
            // Skipping 'empty' and 'fail' matches
            if (0 < result) {
                int nextState = result;
                while (0 < nextState) {
                    tmp.remove_prefix(nextState);
                    nextState = it->matchOffset(tmp);
                    if (-1 < nextState) {
                        result += nextState;
                    }
                }
            }
            return result;
        }
    };

    struct ZeroOrMore {
        SPtr<StrPattern> it;

        int matchOffset(V v) const { return it->matchOffset(v); }
    };

    struct Sequence {
        Vec<StrPattern> it;

        int matchOffset(V v) const {
            int result = 0;
            for (const auto& sub : it) {
                auto tmp = v;
                tmp.remove_prefix(result);
                int offset = sub.matchOffset(tmp);
                if (offset == -1) {
                    return offset;
                } else {
                    result += offset;
                }
            }
            return result;
        }
    };

    struct AnyChar {
        CharSet set;

        AnyChar(CR<CharSet> inSet) : set(inSet){};

        int matchOffset(V v) const {
            if (set.contains(v.front())) {
                return 1;
            } else {
                return -1;
            }
        }
    };


    StrPattern operator+() {
        return StrPattern(OneOrMore{
            .it = SPtr<StrPattern>(new StrPattern(std::move(*this)))});
    }

    StrPattern operator*() {
        return StrPattern(ZeroOrMore{
            .it = SPtr<StrPattern>(new StrPattern(std::move(*this)))});
    }

    using PatternVar = std::variant<OneOrMore, ZeroOrMore, AnyChar>;

    StrPattern(CR<StrPattern> copy) : pattern(copy.pattern) {}
    StrPattern(StrPattern&& moved) : pattern(std::move(moved.pattern)) {}
    StrPattern(PatternVar&& value) : pattern(std::move(value)) {}

    PatternVar pattern;

    int matchOffset(V view) const {
        return std::visit(
            [&view](auto& it) { return it.matchOffset(view); }, pattern);
    }
};


std::ostream& operator<<(std::ostream& os, StrPattern const& value);

std::ostream& operator<<(
    std::ostream&                 os,
    StrPattern::ZeroOrMore const& value) {
    os << "*(" << *(value.it) << ")";
    return os;
}


std::ostream& operator<<(
    std::ostream&              os,
    StrPattern::AnyChar const& value) {
    os << value.set;
    return os;
}

std::ostream& operator<<(
    std::ostream&                os,
    StrPattern::OneOrMore const& value) {
    os << "+(" << *(value.it) << ")";
    return os;
}

std::ostream& operator<<(
    std::ostream&                 os,
    StrPattern::PatternVar const& value) {
    return std::visit(
        [&os](auto& it) -> std::ostream& {
            os << it;
            return os;
        },
        value);
}

std::ostream& operator<<(std::ostream& os, StrPattern const& value) {
    os << value.pattern;
    return os;
}

/// Type constraint for types that can be passed into base methods of the
/// positional string checking such as `.at()` or `.skip()` as well as all
/// helper methods for better skipping such as `skipWhile`
template <typename S>
concept PosStrCheckable = (                                     //
    std::convertible_to<std::remove_cvref_t<S>, char>           //
    || std::convertible_to<std::remove_cvref_t<S>, CharSet>     //
    || std::convertible_to<std::remove_cvref_t<S>, std::string> //
    || std::convertible_to<std::remove_cvref_t<S>, StrPattern>  //
);

struct PosStr {
    PosStr(
        std::string_view inView,
        LineCol          inLoc = {.line = 0, .column = 0},
        int              inPos = 0)
        : view(inView), loc(inLoc), pos(inPos) {}

    struct SliceStartData {
        LineCol loc;
        int     pos;
    };

    Vec<SliceStartData> slices;
    /// Underlying string view
    std::string_view view;
    /// Line and column information for the current position in the string
    /// string view
    LineCol loc;
    /// Absolute offset from the start of string view
    int pos = 0;

    void pushSlice() { slices.push_back({loc, pos}); }

    using AdvanceCb = std::function<void(PosStr&)>;

    std::string_view completeView(CR<SliceStartData> slice) const {
        return std::string_view(view.data() + slice.pos, pos - slice.pos);
    }


    /// Pop last slice into a token object
    template <typename K>
    Token<K> popTok(K kind) {
        return Token<K>{
            .kind = kind, .text = completeView(slices.pop_back_v())};
    }

    template <typename K>
    Token<K> tok(K kind, AdvanceCb cb) {
        pushSlice();
        cb(*this);
        return popTok(kind);
    }

    template <typename K>
    Token<K> tok(K kind, CR<StrPattern> pattern, bool allowEmpty = false) {
        pushSlice();
        skip(pattern, 0, allowEmpty);
        return popTok(kind);
    }

    PosStr popSlice() {
        auto slice = slices.pop_back_v();
        return PosStr(completeView(slice), slice.loc);
    }

    template <typename K>
    Token<K> slice(K kind, AdvanceCb cb) {
        pushSlice();
        cb(*this);
        return popSlice();
    }

    bool hasNext(int shift = 1) const { return pos < view.size(); }

    void next(int count = 1) {
        for (int i = 0; i < count; ++i) {
            ++pos;
            switch (get()) {
                case '\n': {
                    ++loc.line;
                    loc.column = 0;
                    break;
                }
                default: {
                    ++loc.column;
                }
            }
        }
    }

    char get(int offset = 0) const {
        char result = '\0';
        if (pos + offset < view.size()) {
            result = view[pos + offset];
        }
        return result;
    }

    bool finished() { return get() == '\0'; }

    char pop() {
        char result = get();
        next();
        return result;
    }

    /// Check if the current position (with given \arg offset) contains
    /// expected character.
    bool at(char expected, int offset = 0) const {
        return get(offset) == expected;
    }

    bool at(CR<CharSet> expected, int offset = 0) const {
        return expected.contains(get(offset));
    }

    bool at(CR<std::string> expected, int offset = 0) const {
        for (const auto& [idx, ch] : enumerate(expected)) {
            if (get(offset + idx) != ch) {
                return false;
            }
        }
        return true;
    }

    int getSkip(CR<StrPattern> pattern, int offset = 0) const {
        const auto base = std::string_view(
            view.data() + (pos + offset), view.size() - (pos + offset));
        return pattern.matchOffset(base);
    }

    bool at(CR<StrPattern> pattern, int offset = 0) const {
        return (0 <= getSkip(pattern));
    }

    void skip(
        CR<StrPattern> pattern,
        int            offset     = 0,
        bool           allowEmpty = false) {
        auto skip = getSkip(pattern);
        if (0 < skip || (skip == 0 && allowEmpty)) {
            next(skip);
        } else if (skip == 0 && !allowEmpty) {
            throw UnexpectedCharError(
                "Pattern '$#' skipped zero characters at position $#:$#"
                    % to_string_vec(pattern, loc.line, loc.column),
                loc);
        } else {
            throw UnexpectedCharError(
                "Skip of the pattern '$#' failed on $#:$#"
                    % to_string_vec(pattern, loc.line, loc.column),
                loc);
        }
    }

    void skip(char expected) {
        if (get() == expected) {
            next();
        } else {
            throw UnexpectedCharError(
                "Unexpected character encountered during lexing: found "
                "'$#' but expected '$#' on $#:$#"
                    % to_string_vec(get(), expected, loc.line, loc.column),
                loc);
        }
    }


    void skip(std::string expected) {
        if (at(expected)) {
            next(expected.size());
        } else {
            throw UnexpectedCharError(
                "Unexpected character encountered during lexing: found "
                "'$#' but expected '$#' on $#:$#"
                    % to_string_vec(get(), expected, loc.line, loc.column),
                loc);
        }
    }

    void skip(CR<CharSet> expected) {
        if (expected.contains(get())) {
            next();
        } else {
            throw UnexpectedCharError(
                "Unexpected character encountered during lexing: fonud "
                "'$#' but expected any of '$#' on $#:$#"
                    % to_string_vec(get(), expected, loc.line, loc.column),
                loc);
        }
    }

    void skipWhile(const PosStrCheckable auto& item) {
        while (at(item)) {
            next();
        }
    }

    void skipTo(const PosStrCheckable auto& item) {
        while (!at(item)) {
            next();
        }
    }

    void skipBefore(const PosStrCheckable auto& item) {
        while (!at(item, 1)) {
            next();
        }
    }

    void skipPast(const PosStrCheckable auto& item) {
        while (at(item)) {
            next();
        }
    }

    /*!Check string is positioned on the empty line - `\n____\n` where `_`
    is any horizontal space character. Check can be executed at any
    position on the line.
    */
    bool isEmptyLine() {
        auto before = 0;
        while (at(charsets::HorizontalSpace, before)) {
            --before;
        }

        if (!at(before)) {
            return false;
        }

        auto after = 0;
        while (at(charsets::HorizontalSpace, after)) {
            ++after;
        }
        if (!at(after)) {
            return false;
        }
        return true;
    }

    /// Skip to the end of current line. After parsing cursor is positioned
    /// on the last character in the string, or closest newline.
    void skipToEOL() { skipTo(charsets::Newline); }


    /// Skip past the end of the line - that is, for `111\n2222` put cursor
    /// at the first `2` on the second line.
    void skipPastEOL() { skipPast(charsets::Newline); }

    /*! If string is positioned on the empty line skip it, and return
    `true`. Otherwise return `false` */
    bool trySkipEmptyLine() {
        bool result = isEmptyLine();
        if (result) {
            skipPastEOL();
        }
        return result;
    }

    /// Skip any number of horizontal whitespaces starting from the current
    /// position and return a number of spaces skipped.
    int skipIndent(const int& maxIndent = INT_MAX) {
        int result;
        while (at(charsets::HorizontalSpace)) {
            ++result;
            next();
            if (maxIndent <= result) {
                break;
            }
        }
        return result;
    }

    void skipBeforeEOL() { skipBefore(charsets::Newline); }

    bool hasAhead(
        const PosStrCheckable auto& item,
        int                         maxLimit = INT_MAX) {
        bool result;
        int  pos = 0;
        while (hasNext(pos) && pos < maxLimit) {
            if (at(item, pos)) {
                return true;
            }
            ++pos;
        };
        return false;
    }

    /// Get number of horizontal spaces starting from the current position.
    /// NOTE: if string is positioned on the newline or any other vertical
    /// space indentation is considered to be zero. `"\n____text" -> 0`,
    /// but `"____test" -> 4`
    int getIndent() const {
        int result;
        while (at(charsets::HorizontalSpace, result)) {
            ++result;
        }
        return result;
    }

    bool hasMoreIndent(const int& indent, const bool& exactIndent = false)
        const {
        bool result;
        int  foundIndent = 0;
        while (at(charsets::HorizontalSpace, foundIndent)) {
            ++foundIndent;
            if (indent <= foundIndent) {
                break;
            }
        }

        if (foundIndent == indent) {
            return true;
        } else if (foundIndent <= indent) {
            return false;
        } else {
            return !exactIndent
                || at(charsets::HorizontalSpace, foundIndent);
        }
        return result;
    }


    void skipIdent(const CharSet& chars = charsets::IdentChars) {
        skipWhile(chars);
    }

    struct UnbalancedSkipArgs {
        const CharSet& openChars;
        const CharSet& closeChars;
        const CharSet& endChars = charsets::Newline;
        const bool&    doRaise  = true;
        /// Allow use of `\` character to escape special characters
        const bool& allowEscape = true;
        /// whether opening brace had already been skipped by the wrapping
        /// lexer logic. Can be used to provide custom handling for the
        /// opening element. Together with `consumeLast` allow for a fully
        /// custom handling of the outermost wrapping braces.
        const bool& skippedStart = false;
        /// what to do with the wrapping tokens of a balanced range. By
        /// default they are also skipped, but if lexer needs to handle
        /// this case separately you can set this argument to false.
        const bool& consumeLast = true;
    };

    void skipBalancedSlice(CR<UnbalancedSkipArgs> args) {
        auto fullCount = args.skippedStart ? 1 : 0;
        int  count[sizeof(char) * 8];
        while (hasNext()) {
            if (args.allowEscape && at('\\')) {
                next();
                next();
            } else if (at(args.openChars)) {
                ++fullCount;
                ++count[ord(pop())];
            } else if (at(args.closeChars)) {
                --fullCount;
                if ((0 < fullCount) || args.consumeLast) {
                    --count[ord(pop())];
                }
                if (fullCount == 0) {
                    return;
                }
            } else if (at(args.endChars)) {
                if (0 < fullCount) {
                    if (args.doRaise) {
                        throw makeUnexpected(
                            "balanced opening/closing pair",
                            "balanced opening/closing pair");
                    } else {
                        return;
                    }
                } else {
                    return;
                }
            } else {
                next();
            }
        }
        if ((0 < fullCount && args.doRaise)) {
            throw makeUnexpected(
                "balanced opening/closing pair",
                "balanced opening/closing pair");
        }
    }

    /// Create new 'unexpected character' error at the current string
    /// parsing position.
    UnexpectedCharError makeUnexpected(
        CR<std::string> expected, //< What we expected to find?
        CR<std::string> parsing   //< Description of the thing we are
                                  // parsing at the moment
    ) {
        return UnexpectedCharError(
            "Unexpected character encountered during lexing: found '$#' "
            "but expected $# while parsing on $#:$#"
                % to_string_vec(
                    get(), expected, parsing, loc.line, loc.column),
            loc);
    }
};

void skipStringLit(PosStr& str) {
    auto found = false;
    str.next();
    while (!found) {
        found = str.at('"') && !str.at('\\', -1);
        str.next();
    }
}

void skipDigit(R<PosStr> str) {
    if (str.at('-')) {
        str.next();
    }
    if (str.at("0x")) {
        str.next(2);
        str.skip(charsets::HexDigits);
        str.skipWhile(charsets::HexDigits + CharSet{'_'});
    } else if (str.at("0b")) {
        str.next(2);
        str.skip(CharSet{'0', '1'});
        str.skipWhile(CharSet{'0', '1'});
    } else {
        str.skip(charsets::Digits);
        str.skipWhile(charsets::Digits + CharSet{'_', '.'});
    }
}


using Catch::Matchers::EndsWith;
using Catch::Matchers::Message;
using Catch::Matchers::StartsWith;

TEST_CASE("Enumerate") {
    std::string str{"01234"};
    for (const auto& [idx, value] : enumerate(str)) {}
}

TEST_CASE("Vector") {
    SECTION("Slice and indexing operators") {
        Vec<int> v{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

        // Test slice operator with positive indices
        std::span<int> slice1 = v[slice(3, 5)];
        REQUIRE(slice1.size() == 3);
        for (int i = 0; i < 3; ++i) {
            CHECK(slice1[i] == v[i + 3]);
        }

        // Test slice operator with backwards indices
        std::span<int> slice2 = v[slice(3, 3_B)];
        REQUIRE(slice2.size() == 5);
        for (int i = 0; i < 3; ++i) {
            CHECK(slice2[i] == v[i + 3]);
        }

        // Test slice operator with out-of-bounds indices, raise is
        // guaranteed
        REQUIRE_THROWS_AS(v.at(slice(0, 11)), std::out_of_range);
        REQUIRE_THROWS_AS(v.at(slice(12, 14)), std::out_of_range);
        REQUIRE_THROWS_AS(v.at(slice(1, 20_B)), std::out_of_range);

        // Test indexing operator with positive index
        REQUIRE(v[3] == 3);

        REQUIRE(v[3_B] == 7);
        REQUIRE(v[v.size() - 3] == 7);

        REQUIRE_THROWS_AS(v.at(10), std::out_of_range);
        REQUIRE_THROWS_AS(v.at(11_B), std::out_of_range);
    }

    SECTION("Edit data via span view") {
        Vec<int> v(5, 0);

        // Test modification using slice operator
        std::span<int> span = v[slice(1, 3)];
        for (int& x : span) {
            x = 42;
        }
        REQUIRE(v[0] == 0);
        REQUIRE(v[1] == 42);
        REQUIRE(v[2] == 42);
        REQUIRE(v[3] == 42);
        REQUIRE(v[4] == 0);

        // Test modification using indexing operator
        v[1] = 0;
        v[2] = 0;
        v[3] = 0;
        REQUIRE(v[0] == 0);
        REQUIRE(v[1] == 0);
        REQUIRE(v[2] == 0);
        REQUIRE(v[3] == 0);
        REQUIRE(v[4] == 0);
    }
}

TEST_CASE("String formatting", "[str]") {
    SECTION("Plaintext") { REQUIRE("A" % to_string_vec("a") == "A"); }

    SECTION("Basic interpolation fragment parsing") {
        {
            auto f = addfFragments("${A}+${B}");
            REQUIRE(f.size() == 3);
            REQUIRE(f[0].text == "A");
            REQUIRE(f[0].kind == addfExpr);
            REQUIRE(f[1].kind == addfText);
            REQUIRE(f[1].text == "+");
            REQUIRE(f[2].kind == addfExpr);
            REQUIRE(f[2].text == "B");
        }
        {
            auto f = addfFragments("A");
            REQUIRE(f.size() == 1);
            REQUIRE(f[0].kind == addfText);
            REQUIRE(f[0].text == "A");
        }
        {
            auto f = addfFragments("$A");
            REQUIRE(f.size() == 1);
            REQUIRE(f[0].kind == addfVar);
            REQUIRE(f[0].text == "A");
        }
        {
            auto f = addfFragments("${A}");
            REQUIRE(f.size() == 1);
            REQUIRE(f[0].kind == addfExpr);
            REQUIRE(f[0].text == "A");
        }
    }

    SECTION("Interpolate values by index") {
        REQUIRE(to_string_vec("#") == std::vector<std::string>({"#"}));
        REQUIRE("$1" % to_string_vec("#") == "#");
        REQUIRE("$1+$2" % to_string_vec("@", "@") == "@+@");
        // If interpolation placeholder starts with integer value it won't
        // be treated as a name
        REQUIRE("$1A" % to_string_vec("@") == "@A");
        // If you want to make a name that starts with an integer use `${}`
        REQUIRE("${1A}" % to_string_vec("1A", "VALUE") == "VALUE");
        // If element at the required index is not found
        // `FormatStringError` exception is raised. Note that elements use
        // zero-based indexing.
        REQUIRE_THROWS_MATCHES(
            "$1000" % to_string_vec(),
            FormatStringError,
            Message(
                "Argument index out of bounds. Accessed [999], but only "
                "0 arguments were supplied"));
    }

    SECTION("Interpolate values by names") {
        REQUIRE("$name" % to_string_vec("name", "VALUE") == "VALUE");
        REQUIRE("${name}" % to_string_vec("name", "VALUE") == "VALUE");
        REQUIRE(
            "${name}*${name}" % to_string_vec("name", "VALUE")
            == "VALUE*VALUE");

        REQUIRE_THROWS_MATCHES(
            "${RANDOM}" % to_string_vec(),
            FormatStringError,
            Message("No interpolation argument named 'RANDOM'"));
    }
}

TEST_CASE("Test integral set operations", "[contains]") {
    IntSet<char> s;
    IntSet<char> other;
    SECTION("Initial set content") {
        // Default-initialized set does not contain any values and has
        // `.size()` zero
        REQUIRE(s.size() == 0);
        // You can include values to the set using `.incl` method
        s.incl('a');
        // `.size()` shows the number of values included in the set
        REQUIRE(s.size() == 1);
    }

    SECTION("Contains for a single item") {
        // Presence of specific element can be tested using `.contains()`
        // value
        REQUIRE(!s.contains('c'));
        // Elements can be added
        s.incl('c');
        // Tested again
        REQUIRE(s.contains('c'));
        // Then excluded
        s.excl('c');
        // And then tested again, now for absence
        REQUIRE(!s.contains('c'));
    }

    SECTION("Contains for set operations") {
        // You can check for subset relation as well. Default (empty)  sets
        // are subset of each other
        REQUIRE(s.contains(other));
        REQUIRE(other.contains(s));
        // Since they both have the same elements
        REQUIRE(other.size() == 0);
        REQUIRE(s.size() == 0);
        // After adding element to the set  this no longer holds
        s.incl('s');
        REQUIRE(s.contains(other));
        REQUIRE(!other.contains(s));
        // You can test for subset relation using `<` operator: it tests
        // for proper subset
        REQUIRE(other < s);
        // You can also test for a regular subset operation, using `<=`
        // operator which is analogous to the `s.contains(other)`
        REQUIRE(other <= s);
        other.incl('s');
        // After including the same element in the set `<` no longer holds
        REQUIRE(!(other < s));
        // But regular subset check is ok
        REQUIRE(other <= s);
        // And it now works in both directions again
        REQUIRE(s <= other);
    }


    SECTION("Set operations") {
        REQUIRE((s + other).size() == 0);
        REQUIRE((s - other).size() == 0);
        REQUIRE(s < (s + IntSet<char>{'1'}));
        REQUIRE(s < (s | IntSet<char>{'1'}));
        REQUIRE(s == (s & IntSet<char>{'1'}));
    }

    SECTION("Integer set operators") {
        IntSet<i8> s1{1_i8, 2_i8, 3_i8};
        IntSet<i8> s2{2_i8, 3_i8, 4_i8};
        IntSet<i8> s3{4_i8, 5_i8, 6_i8};
        IntSet<i8> empty;

        // Check that the union of s1 and s2 is {1, 2, 3, 4}
        REQUIRE((s1 + s2) == IntSet<i8>{1_i8, 2_i8, 3_i8, 4_i8});
        // Check that the intersection of s1 and s2 is {2, 3}
        REQUIRE((s1 & s2) == IntSet<i8>{2_i8, 3_i8});
        // Check that the difference between s1 and s2 is {1}
        REQUIRE((s1 - s2) == IntSet<i8>{1_i8});
        // Check that the symmetric difference between s1 and s2 is {1, 4}
        REQUIRE((s1 ^ s2) == IntSet<i8>{1_i8, 4_i8});
        // Check that the union of s1 and s3 is {1, 2, 3, 4, 5, 6}
        REQUIRE(
            (s1 + s3) == IntSet<i8>{1_i8, 2_i8, 3_i8, 4_i8, 5_i8, 6_i8});
        // Check that the intersection of s1 and s3 is empty
        REQUIRE((s1 & s3).empty());
        // Check that the difference between s1 and s3 is {1, 2, 3}
        REQUIRE((s1 - s3) == s1);
        // Check that the symmetric difference between s1 and s3 is {1, 2,
        // 3, 4, 5, 6}
        REQUIRE((s1 ^ s3) == (s1 + s3));
        // Check that the union of empty and s1 is {1, 2, 3}
        REQUIRE((empty + s1) == s1);
        // Check that the intersection of empty and s1 is empty
        REQUIRE((empty & s1).empty());
        // Check that the difference between empty and s1 is empty
        REQUIRE((empty - s1).empty());
        // Check that the symmetric difference between empty and s1 is {1,
        // 2, 3}
        REQUIRE((empty ^ s1) == s1);
    }
}

TEST_CASE("Positional string cursor movements", "[str]") {
    std::string base{"01234"};
    PosStr      str{base};
    SECTION("Check for characters on the position ahead") {
        REQUIRE(str.at('0'));
        REQUIRE(str.at('1', 1));
        REQUIRE(str.at('2', 2));
    }

    SECTION("Check for character set on the position ahead") {
        REQUIRE(str.at({slice('0', '9')}));
        REQUIRE(str.at(charsets::Digits));
        REQUIRE(!str.at(charsets::Letters));
    }

    SECTION("Skip while single character") {
        str.skipWhile('0');
        REQUIRE(str.at('1'));
    }

    SECTION("Skip while character set") {
        REQUIRE(str.at('0'));
        str.skipWhile(CharSet{'0', '1', '2'});
        REQUIRE(str.at('3'));
    }

    SECTION("Skip before a character") {
        str.skipBefore('3');
        REQUIRE(str.at('2'));
    }

    SECTION("Skip to a character") {
        str.skipTo('3');
        REQUIRE(str.at('3'));
    }

    SECTION("Skip until one of the caracters") {
        str.skipBefore(CharSet{'3', '4'});
        REQUIRE(str.at('2'));
    }

    SECTION("Skip until a string is found") {
        str.skipBefore("34");
        REQUIRE(str.at('2'));
    }
}


TEST_CASE("Positional string slice construction", "[str]") {
    SECTION("Pop until slice") {
        PosStr s0{"[123]"};
        REQUIRE(s0.get() == '[');
        s0.skipTo(']');
        REQUIRE(s0.get() == ']');

        PosStr s1{"[123]"};
        REQUIRE(s1.get() == '[');
        {
            s1.pushSlice();
            s1.skipTo(']');
            auto slice = s1.popSlice();
            REQUIRE(slice.view == "[123");
        }
    }


    SECTION("Token construction using callbacks") {
        PosStr s{"0123*"};
        auto   tok = s.tok<int>(
            0, [](PosStr& str) { str.skipWhile(charsets::Digits); });
        REQUIRE(tok.text == "0123");
    }

    SECTION("Token constrution using standalone functions") {
        PosStr s{R"("string"/123/0x123)"};
        {
            auto tok = s.tok(0, skipStringLit);
            REQUIRE(tok.text == "\"string\"");
            s.skip('/');
        }
        {
            auto tok = s.tok(0, skipDigit);
            REQUIRE(tok.text == "123");
            s.skip('/');
        }
        {
            auto tok = s.tok(0, skipDigit);
            REQUIRE(tok.text == "0x123");
        }
    }
}

TEST_CASE("Positional strig DSL matches") {
    std::cout << "hello motherfucker\n";
    SECTION("Character set match") {
        PosStr     s{"01234"};
        StrPattern first{StrPattern::AnyChar{{slice('0', '1'), '2'}}};
        StrPattern second{StrPattern::AnyChar{{'3', '4'}}};
        {
            auto tok = s.tok(0, +first);
            REQUIRE(tok.text == "012");
        }
        {
            auto tok = s.tok(0, +second);
            REQUIRE(tok.text == "34");
        }
    }
}


const auto otcSubtreeKinds = IntSet<OrgTextContext>{
    slice(otcSubtree0, otcSubtreeOther)};
const auto otcMarkupKinds = IntSet<OrgTextContext>{
    slice(otcBold, otcMonospaceBlock)};

const auto orgMarkupKinds = IntSet<OrgNodeKind>{
    orgBold,
    orgItalic,
    orgVerbatim,
    orgBacktick,
    orgUnderline,
    orgStrike,
    orgQuote,
    orgAngle,
    orgMonospace};

const auto orgLineCommandKinds = IntSet<OrgNodeKind>{
    slice(orgCommandTitle, orgCommandCaption),
    orgAttrImg};

/// Line commands that can be used as associted property elements.
const auto orgAttachableKinds = IntSet<OrgNodeKind>{
    orgCommandAttrHtml,
    orgCommandName,
    orgCommandHeader,
    orgAttrImg,
    orgCommandCaption};

const auto orgBlockCommandKinds = IntSet<OrgNodeKind>{
    orgTable,
    orgSrcCode,
    orgQuoteBlock,
    orgExample};

/// Line or block commands that can have associated property elements
const auto orgAssociatedKinds = IntSet<OrgNodeKind>{orgLink}
                              + orgBlockCommandKinds
                              + IntSet<OrgNodeKind>{
                                  orgCommandInclude,
                                  orgResult};

/// Line commands that cannot be used in standalone manner, and always
/// have to be associated with some other block/line command
const auto orgNoAssociatedKinds = IntSet<OrgNodeKind>{
    orgCommandHeader,
    orgCommandName,
    orgCommandCaption};

/// Nodes that should only be processed when encountered on the toplevel
/// (initial document configuration)
const auto orgDoclevelKinds = IntSet<OrgNodeKind>{
    orgCommandOptions,
    orgCommandTitle,
    orgCommandAuthor,
    orgCommandBackendOptions};


/// All node kinds that should not have subnode values
const auto orgTokenKinds = IntSet<OrgNodeKind>{
    orgCmdKey,        orgTarget,
    orgTextSeparator, orgRawLink,
    orgRadioTarget,   orgCmdFlag,
    orgOrgTag,        orgCodeText,
    orgSubtreeStars,  orgSpace,
    orgPunctuation,   orgAtMention,

    orgIdent,         orgBullet,
    orgBareIdent,     orgRawText,
    orgUnparsed,      orgBigIdent,
    orgUrgencyStatus, orgVerbatimMultilineBlock,
    orgWord,          orgEscaped,
    orgNewline,       orgComment,
    orgCheckbox,      orgCounter,
    orgCompletion,    orgTimeStamp,
    orgSimpleTime,    orgEmpty};


/// ALl types of org-mode values that can have subnodes
const auto orgSubnodeKinds = IntSet<OrgNodeKind>{slice(orgNone, org__LAST)}
                           - orgTokenKinds
                           - IntSet<OrgNodeKind>{orgUserNode};

/// All possible values of the org-node kind
const auto orgAllKinds = IntSet<OrgNodeKind>{slice(orgNone, org__LAST)};


/// Nodes that can only contain fixed number of subnodes of fixed kinds.
const auto orgTokenLikeKinds = IntSet<OrgNodeKind>{
    orgTimeRange,
    orgTimeStamp};


/// Nodes that act like containers but are strongly typed and store
/// subelements in many typed fields instead of one long untyped list.
const auto orgTypedContainerLikeKinds = IntSet<OrgNodeKind>{
    orgListItem,
    orgSubtree};

/// Nodes that can contain any number of nested nodes in any combinations.
const auto orgContainerLikeKinds = IntSet<OrgNodeKind>{
    orgStmtList,
    orgParagraph,
    orgList,
    orgLink,
};

struct OrgTokenStore;
struct OrgNodeStore;

struct OrgNode {
    static OrgTokenStore* tokens;
    static OrgNodeStore*  nodes;

    OrgNodeKind kind;
    /// Id of the token group this node refers to (in case of multple
    /// documents that were parsed by the same application)
    u16 storeId;
    /// Left index of the node -- depending on the specific kind it might
    /// refer to the leftmost subnode of the current node or specific token
    /// in the token store.
    u32 lhs;
    /// Right index of the node -- for non-token nodes referes to the index
    /// of the rightmost node
    u32 rhs;
};

using OrgToken = Token<OrgTokenKind>;

struct OrgTokenGroup {
    Vec<OrgToken> tokens;
    void          push(CR<OrgToken> tok) { tokens.push_back(tok); }
};

struct OrgTokenStore {
    Vec<OrgTokenGroup> groups;
};

struct OrgNodeGroup {
    Vec<OrgNode> nodes;
};

struct OrgNodeStore {
    Vec<OrgNodeGroup> groups;
};


struct OrgLexer {
    std::string    base;
    PosStr         str;
    OrgTokenGroup* out;
    void           push(CR<OrgToken> tok) { out->push(tok); }

    void lexAngle() {
        if (str.at("<%%")) {
            push(str.tok(OTkDiaryTime, [](PosStr& str) {
                str.skip("<%%");
                str.skipBalancedSlice(
                    {.openChars  = CharSet{'('},
                     .closeChars = CharSet{')'}});
                str.skip(">");
            }));
        } else if (str.at("<<<")) {
            push(str.tok(
                OTkTripleAngleOpen, [](PosStr& str) { str.next(3); }));
        }
    }
};

int main(int argc, const char** argv) {
    return Catch::Session().run(argc, argv);
}
