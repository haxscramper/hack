#include <span>
#include <variant>
#include <iostream>
#include <bitset>
#include <vector>
#include <optional>
#include <utility>
#include <memory>
#include <functional>
#include <array>

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

template <typename T>
CR<T> cr(CR<T> in) {
   return in;
}

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
concept ImplementsOrd = requires(CR<T> value) {
    { ord(value) } -> std::same_as<int>;
};


struct ImplementError : public std::runtime_error {
    explicit ImplementError(const std::string& message = "")
        : std::runtime_error(message) {}
};


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

template <typename T, int Size>
struct Array : std::array<T, Size> {
    using std::array<T, Size>::array; // Inherit constructor
    using std::array<T, Size>::size;
    using std::array<T, Size>::at;
    using std::array<T, Size>::operator[];
    using std::array<T, Size>::back;
    using std::array<T, Size>::begin;
    using std::array<T, Size>::end;

    operator R<std::array<T, Size>>() {
        return static_cast<std::array<T, Size>>(*this);
    }

    operator CR<std::array<T, Size>>() const {
        return static_cast<std::array<T, Size>>(*this);
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

    int high() const { return size() - 1; }
    int indexOf(CR<T> item) const { return index_of(*this, item); }
};

template <ImplementsOrd Key, typename Val>
struct TypArray : public Array<Val, 8 * sizeof(Key)> {
    TypArray(std::initializer_list<Pair<Key, Val>> items) {
        for (const auto& [key, val] : items) {
            at(key) = val;
        }
    }

    Val& at(CR<Key> value) { return at(ord(value)); }
};

struct MarkupConfigPair {
    OrgTokenKind startKind;
    OrgTokenKind finishKind;
    OrgTokenKind inlineKind;
};

/// Table of the markup config information, to reduce usage of the
/// character literals directly in the code.
const TypArray<char, MarkupConfigPair> markupConfig{{
    {'*', {OTkBoldOpen, OTkBoldClose, OTkBoldInline}},
    {'/', {OTkItalicOpen, OTkItalicClose, OTkItalicInline}},
    {'=', {OTkVerbatimOpen, OTkVerbatimClose, OTkVerbatimInline}},
    {'`', {OTkBacktickOpen, OTkBacktickClose, OTkBacktickInline}},
    {'~', {OTkMonospaceOpen, OTkMonospaceClose, OTkMonospaceInline}},
    {'_', {OTkUnderlineOpen, OTkUnderlineClose, OTkUnderlineInline}},
    {'+', {OTkStrikeOpen, OTkStrikeClose, OTkStrikeInline}},
    {'"', {OTkQuoteOpen, OTkQuoteClose, otNone}},
}};


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
    using std::vector<T>::vector; // Inherit constructor from
                                  // std::vector
    using std::vector<T>::size;
    using std::vector<T>::at;
    using std::vector<T>::operator[];
    using std::vector<T>::push_back;
    using std::vector<T>::back;
    using std::vector<T>::pop_back;
    using std::vector<T>::begin;
    using std::vector<T>::end;
    using std::vector<T>::insert;

    void append(CR<Vec<T>> other) {
        insert(end(), other.begin(), other.end());
    }

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

C<CharSet> TextChars = MaybeLetters + Digits + CharSet{'.', ',', '-'};
} // namespace charsets

const CharSet markupKeys{'*', '/', '=', '`', '~', '_', '+', '"'};


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

    /// Check if token text is a view over real data
    bool hasData() const { return text.data() != nullptr; }
    /// Return character count for the token. If it does not contain any
    /// data return 0.
    int size() const {
        if (hasData()) {
            return text.size();
        } else {
            return 0;
        }
    }


    /// Return offset from the starting point of the string. If token does
    /// not have real data, return faked position (`.size()` of the text)
    /// instead. \warning This function is intended to be used with real
    /// starting point of the view that was used in the originating
    /// positional string and so the behavior with 'fake' token is going to
    /// be invalid when used with any other position in the string.
    std::size_t offsetFrom(const char* start) const {
        if (hasData()) {
            return std::distance(text.data(), start);
        } else {
            return text.size();
        }
    }
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
    int  getPos() const { return pos; }
    void setPos(int _pos) { pos = _pos; }

    using AdvanceCb = std::function<void(PosStr&)>;
    struct Offset {
        int start;
        int end;
        Offset(int _start = 0, int _end = 0) : start(_start), end(_end) {}
    };


    std::string_view completeView(
        CR<SliceStartData> slice,
        Offset             offset = Offset()) const {
        return std::string_view(
            view.data() + slice.pos + offset.start,
            pos - slice.pos + offset.end);
    }


    template <typename K>
    Token<K> fakeTok(K kind, Offset offset = Offset()) {
        return Token<K>{.kind = kind};
    }

    /// Pop last slice into a token object
    template <typename K>
    Token<K> popTok(K kind, Offset offset = Offset()) {
        return Token<K>{
            .kind = kind,
            .text = completeView(slices.pop_back_v(), offset)};
    }

    PosStr popSlice(Offset offset = {}) {
        auto slice = slices.pop_back_v();
        return PosStr(completeView(slice, offset), slice.loc);
    }


    template <typename K>
    Token<K> tok(
        K              kind,
        CR<StrPattern> pattern,
        bool           allowEmpty = false,
        Offset         offset     = {}) {
        pushSlice();
        skip(pattern, 0, allowEmpty);
        return popTok(kind, offset);
    }


    template <typename... Args>
    using AdvanceHandler = void(PosStr&, Args...);

    template <typename K, typename... Args>
    Token<K> tok(
        K                       kind,
        Offset                  offset,
        AdvanceHandler<Args...> cb,
        Args&&... args) {
        pushSlice();
        cb(*this, std::forward<Args>(args)...);
        return popTok(kind, offset);
    }

    template <typename... Args>
    PosStr slice(
        Offset                  offset,
        AdvanceHandler<Args...> cb,
        Args&&... args) {
        pushSlice();
        cb(*this, std::forward<Args>(args)...);
        return popSlice(offset);
    }

    template <typename K, typename... Args>
    Token<K> tok(K kind, AdvanceHandler<Args...> cb, Args&&... args) {
        pushSlice();
        cb(*this, std::forward<Args>(args)...);
        return popTok(kind);
    }

    template <typename... Args>
    PosStr slice(AdvanceHandler<Args...> cb, Args&&... args) {
        pushSlice();
        cb(*this, std::forward<Args>(args)...);
        return popSlice();
    }


    template <typename K>
    Token<K> tok(K kind, AdvanceCb cb, Offset offset = Offset()) {
        pushSlice();
        cb(*this);
        return popTok(kind, offset);
    }

    PosStr slice(AdvanceCb cb, Offset offset = Offset()) {
        pushSlice();
        cb(*this);
        return popSlice(offset);
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

    bool finished() const { return get() == '\0'; }
    bool atStart() const { return pos == 0; }
    bool beforeEnd() const { return !hasNext(1); }

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

    void space() { skipWhile(charsets::HorizontalSpace); }

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


struct BalancedSkipArgs {
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

void skipBalancedSlice(PosStr& str, CR<BalancedSkipArgs> args) {
    auto fullCount = args.skippedStart ? 1 : 0;
    int  count[sizeof(char) * 8];
    while (str.hasNext()) {
        if (args.allowEscape && str.at('\\')) {
            str.next();
            str.next();
        } else if (str.at(args.openChars)) {
            ++fullCount;
            ++count[ord(str.pop())];
        } else if (str.at(args.closeChars)) {
            --fullCount;
            if ((0 < fullCount) || args.consumeLast) {
                --count[ord(str.pop())];
            }
            if (fullCount == 0) {
                return;
            }
        } else if (str.at(args.endChars)) {
            if (0 < fullCount) {
                if (args.doRaise) {
                    throw str.makeUnexpected(
                        "balanced opening/closing pair",
                        "balanced opening/closing pair");
                } else {
                    return;
                }
            } else {
                return;
            }
        } else {
            str.next();
        }
    }
    if ((0 < fullCount && args.doRaise)) {
        throw str.makeUnexpected(
            "balanced opening/closing pair",
            "balanced opening/closing pair");
    }
}


void skipPastEOF(PosStr& str) { assert(false && "IMPLEMENT"); }
void skipCount(PosStr& str, int count) { str.next(count); }
void skipBefore(PosStr& str, char item) { str.skipBefore(item); }
void skipTo(PosStr& str, char item) { str.skipTo(item); }
void skipOne(PosStr& str, char item) { str.skip(item); }
void skipWhile(PosStr& str, char item) { str.skipWhile(item); }

void skipBefore(PosStr& str, const PosStrCheckable auto& item) {
    str.skipBefore(item);
}
void skipWhile(PosStr& str, const PosStrCheckable auto& item) {
    str.skipWhile(item);
}
void skipTo(PosStr& str, const PosStrCheckable auto& item) {
    str.skipTo(item);
}
void skipPast(PosStr& str, const PosStrCheckable auto& item) {
    str.skipPast(item);
}
void skipOne(PosStr& str, const PosStrCheckable auto& item) {
    str.skip(item);
}

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

    void push(CR<OrgToken> tok) { tokens.push_back(tok); }
    void push(CR<Vec<OrgToken>> tok) { tokens.append(tok); }
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


CR<CharSet> OIdentChars{
    slice('a', 'z'),
    slice('A', 'Z'),
    '_',
    '-',
    slice('0', '9')};
CR<CharSet> OIdentStartChars = charsets::IdentChars
                             - CharSet{'_', '-', slice('0', '9')};

// IDEA in figure some additional unicode handing might be performed, but
// for now I just asume text is in UTF-8 and everything above 127 is a
// unicode rune too.

CR<CharSet> OWordChars = CharSet{
    slice('a', 'z'),
    slice('A', 'Z'),
    slice('0', '9'),
    slice('\x7F', '\xFF')};


CR<CharSet> OBigIdentChars  = CharSet{slice('A', 'Z')};
const char  OEndOfFile      = '\x00';
CR<CharSet> OBareIdentChars = charsets::AllChars - charsets::Whitespace;
CR<CharSet> OWhitespace     = charsets::Whitespace - CharSet{'\n'};
CR<CharSet> OEmptyChars     = OWhitespace + CharSet{OEndOfFile};
CR<CharSet> OLinebreaks     = charsets::Newlines + CharSet{OEndOfFile};
CR<CharSet> OMarkupChars    = CharSet{'*', '_', '/', '+', '~', '`'};
CR<CharSet> OVerbatimChars  = CharSet{'`', '~', '='};
CR<CharSet> OPunctChars = CharSet{'(', ')', '[', ']', '.', '?', '!', ','};
CR<CharSet> OPunctOpenChars    = CharSet{'(', '[', '{', '<'};
CR<CharSet> OPunctCloseChars   = CharSet{')', ']', '}', '>'};
CR<CharSet> ONumberedListChars = CharSet{slice('0', '9')}
                               + CharSet{slice('a', 'z')}
                               + CharSet{slice('A', 'Z')};
CR<CharSet> OBulletListChars = CharSet{'-', '+', '*'};
CR<CharSet> OListChars       = ONumberedListChars + OBulletListChars;


struct OrgLexer {
    std::string    base;
    OrgTokenGroup* out;

    void push(CR<OrgToken> tok) { out->push(tok); }
    void push(CR<Vec<OrgToken>> tok) { out->push(tok); }

    void lexAngle(PosStr& str) {
        if (str.at("<%%")) {
            push(str.tok(OTkDiaryTime, [](PosStr& str) {
                str.skip("<%%");
                skipBalancedSlice(
                    str,
                    {.openChars  = CharSet{'('},
                     .closeChars = CharSet{')'}});
                str.skip(">");
            }));
        } else if (str.at("<<<")) {
            push(str.tok(OTkTripleAngleOpen, skipCount, 3));
        } else if (str.at("<<")) {
            push(str.tok(OTkDoubleAngleOpen, skipCount, 2));
            push(str.tok(OTkRawText, skipTo, '>'));
            push(str.tok(OTkRawText, skipOne, ">>"));
        } else if (str.at(charsets::Digits, 1)) {
            auto skipAngles = [](PosStr& str) {
                str.skip('<');
                str.skipTo('>');
                str.skip('>');
            };

            push(str.tok(OTkAngleTime, skipAngles));

            if (str.at("--")) {
                push(str.tok(OTkTimeDash, skipCount, 2));
                push(str.tok(OTkAngleTime, skipAngles));
            }
        } else {
            push(str.tok(OTkAngleOpen, skipCount, 1));
            push(str.tok(OTkRawText, skipTo, '>'));
            push(str.tok(OTkAngleClose, skipOne, '>'));
        }
    }

    void lexTime(PosStr& str) {
        if (str.at('<')) {
            lexAngle(str);
        } else if (str.at('[')) {
            auto skipBracket = [](PosStr& str) {
                str.skip('[');
                str.skipTo(']');
                str.skip(']');
            };

            push(str.tok(OTkBracketTime, skipBracket));
            if (str.at("--")) {
                push(str.tok(OTkTimeDash, skipCount, 2));
                push(str.tok(OTkBracketTime, skipBracket));
            }
        } else {
            throw str.makeUnexpected("'<' or '['", "time");
        }
    }

    void lexLinkTarget(PosStr& str) {
        if (str.at(R"(https)") || str.at(R"(http)")) {
            assert(false && "FIXME");
        } else if (
            str.at(R"(file)")          //
            || str.at(R"(attachment)") //
            || str.at(R"(docview)")    //
            || str.at('/')             //
            || str.at(R"(./)")) {

            if (str.at('.') || str.at('/')) {
                assert(false && "FIXME");
                // push(
                //     result,
                //     str,
                //     result.add(
                //         initFakeTok(str, OTkLinkProtocol, R"(file)")));
            } else {
                push(str.tok(OTkLinkProtocol, skipTo, ':'));
                str.skip(':');
            }

            push(str.tok(OTkLinkTarget, [](PosStr& str) {
                while (!str.finished() && !str.at(R"(::)")) {
                    str.next();
                }
            }));

            if (str.at(R"(::)")) {
                push(str.tok(OTkLinkExtraSeparator, skipCount, 2));
                push(str.tok(OTkLinkExtra, skipPastEOF));
            }
        } else {
            if (str.hasAhead(':')) {
                push(str.tok(OTkLinkProtocol, skipTo, ':'));
                str.skip(':');
                push(str.tok(OTkLinkTarget, skipPastEOF));
            } else {
                push(str.tok(OTkLinkInternal, skipPastEOF));
            }
        }
    }

    void lexBracket(PosStr& str) {
        if (str.at(R"([[)")) {
            push(str.tok(OTkLinkOpen, skipOne, '['));
            // link_token
            {
                push(str.tok(OTkLinkTargetOpen, skipOne, '['));
                PosStr target = str.slice(skipBefore, ']');
                lexLinkTarget(target);
                push(str.tok(OTkLinkTargetClose, skipOne, ']'));
            };
            // description_token
            {
                if (str.at('[')) {
                    push(str.tok(OTkLinkDescriptionOpen, skipOne, '['));
                    PosStr desc = str.slice([](PosStr& str) {
                        int count = 0;
                        while (!str.finished()
                               && (!str.at(']') || (0 < count))) {

                            if (str.at('[')) {
                                ++count;
                            }
                            if (str.at(']')) {
                                --count;
                            }
                            str.next();
                        }
                    });

                    while (!desc.finished()) {
                        lexText(desc);
                    }

                    push(str.tok(OTkLinkDescriptionClose, skipOne, ']'));
                }
            }
            push(str.tok(OTkLinkClose, skipOne, ']'));
        } else if (str.at(R"([fn:)")) {
            push(str.tok(OTkFootnoteStart, skipOne, "[fn"));
            if (str.at(R"(::)")) {
                push(str.tok(OTkDoubleColon, skipOne, R"(::)"));
                // FIXME
                // result.addExpandTok(str, OTkText, str.skipTo(']'););
            } else {
                push(str.tok(OTkColon, skipOne, ':'));
                push(str.tok(OTkIdent, skipTo, ']'));
            }
            push(str.tok(OTkFootnoteEnd, skipOne, ']'));
        } else {
            // FIXME
            // push(trySpecific(str, OTkPunctuation, 1, lexTime));
        }
    }

    void lexTextChars(PosStr& str) {
        bool isStructure = false;
        auto skipCurly   = [](PosStr& str) {
            skipBalancedSlice(
                str,
                BalancedSkipArgs{.openChars = {'{'}, .closeChars = {'}'}});
        };

        auto skipParen = [](PosStr& str) {
            skipBalancedSlice(
                str,
                BalancedSkipArgs{.openChars = {'('}, .closeChars = {')'}});
        };

        auto skipBrace = [](PosStr& str) {
            skipBalancedSlice(
                str,
                BalancedSkipArgs{.openChars = {'['}, .closeChars = {']'}});
        };

        if (str.at("src[_-]?\\w+(\\[|\\{)")) {
            const auto    pos = str.getPos();
            Vec<OrgToken> buf;
            // Starting `src_` prefix
            {
                buf.push_back(str.tok(OTkSrcOpen, skipOne, "src"));
                if (str.at(CharSet{'_', '-'})) {
                    str.next();
                }
            }

            if (str.at(charsets::IdentStartChars)) {
                // FIXME push buffer only if the whole sequence is
                // determined to be a valid structure
                push(buf);
                push(str.tok(OTkSrcName, skipWhile, charsets::IdentChars));
                if (str.at('[')) {
                    push(str.tok(OTkSrcArgs, skipBrace, {1, -2}));
                }

                push(str.tok(OTkSrcBody, skipCurly, {1, -2}));
                push(str.fakeTok(OTkSrcClose));
                isStructure = true;
            } else {
                str.setPos(pos);
            }

        } else if (str.at("call[_-]?\\w+(\\[|\\{)")) {
            const auto    pos = str.getPos();
            Vec<OrgToken> buf;
            buf.push_back(str.tok(OTkCallOpen, skipOne, "call"));
            if (str.at(CharSet{'_', '-'})) {
                str.next();
            }
            if (str.at(charsets::IdentStartChars)) {
                push(buf);
                push(str.tok(OTkSrcName, skipWhile, charsets::IdentChars));
                if (str.at('[')) {
                    push(str.tok(OTkCallInsideHeader, skipBrace, {1, -2}));
                };
                push(str.tok(OTkCallArgs, skipParen, {1, -2}));
                push(str.fakeTok(OTkCallClose));
                isStructure = true;
            } else {
                str.setPos(pos);
            }
        } else if (str.at("https://") || str.at("http://")) {
            push(str.tok(OTkRawUrl, skipBefore, charsets::Whitespace));
        }
        if (!isStructure) {
            bool allUp = true;
            str.pushSlice();
            while (!str.finished()
                   && str.at(charsets::TextChars + CharSet{'-'})) {
                if (!str.at(charsets::HighAsciiLetters)) {
                    allUp = false;
                }
                str.next();
            }
            push(str.popTok(allUp ? OTkBigIdent : OTkWord));
        }
    }

    void lexParenArguments(PosStr& str) {
        push(str.tok(OTkParOpen, skipOne, '('));
        while (!str.at(')')) {
            push(str.tok(OTkRawText, skipBefore, cr(CharSet{',', ')'})));
            if (str.at(',')) {
                push(str.tok(OTkComma, skipOne, ','));
            }
            str.space();
        }
        push(str.tok(OTkParOpen, skipOne, ')'));
    };

    /*!Lex single text entry starting at current position
     */
    void lexText(PosStr& str) {
        const auto NonText = charsets::TextLineChars
                           - charsets::AsciiLetters - charsets::Utf8Any
                           + CharSet{'\n', '/'};

        switch (str.get()) {
            case '\n': {
                push(str.tok(OTkNewline, skipCount, 1));
                break;
            }
            case ' ': {
                push(str.tok(OTkSpace, [](PosStr& str) {
                    while (!str.finished() && str.at(' ')) {
                        str.next();
                    }
                }));
                break;
            }
            case '#': {
                std::function<Vec<OrgToken>(PosStr & str)> rec;
                rec = [&rec](PosStr& str) -> Vec<OrgToken> {
                    Vec<OrgToken> result;
                    result.push_back(str.tok(OTkHashTag, [](PosStr& str) {
                        if (str.at('#')) {
                            str.skip('#');
                        };
                        str.skipWhile(charsets::IdentChars);
                    }));

                    while (str.at(R"(##)") && !str.at(R"(##[)")) {
                        result.push_back(
                            str.tok(OTkHashTagSub, skipOne, '#'));
                        result.push_back(
                            str.tok(OTkHashTag, [](PosStr& str) {
                                str.skip('#');
                                str.skipWhile(charsets::IdentChars);
                            }));
                    }

                    if (str.at(R"(##[)")) {
                        result.push_back(
                            str.tok(OTkHashTagSub, skipOne, '#'));
                        result.push_back(
                            str.tok(OTkHashTagOpen, skipOne, "#["));

                        while (!str.finished() && !str.at(']')) {
                            result.append(rec(str));
                            str.space();
                            if (str.at(',')) {
                                result.push_back(
                                    str.tok(OTkComma, skipOne, ','));
                                str.space();
                            }
                        }
                        result.push_back(
                            str.tok(OTkHashTagClose, skipOne, ']'));
                    }
                    return result;
                };

                push(rec(str));
                break;
            }
            case '@': {
                const auto AtChars = charsets::IdentChars
                                   + charsets::Utf8Any;
                if (str.at(AtChars, 1)) {
                    push(str.tok(OTkAtMention, [&AtChars](PosStr& str) {
                        str.skip('@');
                        str.skipWhile(AtChars);
                    }));
                } else {
                    push(str.tok(OTkPunctuation, skipCount, 1));
                }
                break;
            }
            case '$': {
                auto          tmp = str;
                Vec<OrgToken> buf;
                try {
                    if (tmp.at('$', 1)) {
                        buf.push_back(
                            tmp.tok(OTkDollarOpen, skipOne, "$$"));
                        tmp.pushSlice();
                        bool hasEnd = false;
                        while (!tmp.finished() && !hasEnd) {
                            while (!tmp.finished() && !tmp.at('$')) {
                                tmp.next();
                            }
                            if (tmp.at("$$")) {
                                buf.push_back(
                                    tmp.popTok(OTkLatexInlineRaw));
                                hasEnd = true;
                            } else {
                                throw ImplementError();
                            }
                        }
                        // FIXME
                        // buf.add(tmp.tok(skip OTkDollarClose, '$', '$'));
                    } else {
                        buf.push_back(
                            tmp.tok(OTkDollarOpen, skipOne, '$'));
                        buf.push_back(
                            tmp.tok(OTkLatexInlineRaw, skipBefore, '$'));
                        buf.push_back(
                            tmp.tok(OTkDollarClose, skipOne, '$'));
                    }
                    push(buf);
                    str = tmp;
                } catch (UnexpectedCharError& err) {
                    push(str.tok(OTkPunctuation, skipWhile, '$'));
                }
                break;
            }
            case '\\': {
                switch (str.get(1)) {
                    case '[':
                    case '(': {
                        const auto isInline = str.at('(', 1);
                        if (isInline) {
                            push(str.tok(
                                OTkLatexParOpen, skipOne, R"(\\()"));
                        } else {
                            push(str.tok(
                                OTkLatexBraceOpen, skipOne, R"(\\[)"));
                        }
                        push(str.tok(
                            OTkLatexInlineRaw, [&isInline](PosStr& str) {
                                while (!str.at(
                                    isInline ? R"(\\))" : R"(\\])")) {
                                    str.next();
                                }
                            }));
                        if (isInline) {
                            push(str.tok(OTkLatexParClose, skipOne, ")"));
                        } else {
                            push(
                                str.tok(OTkLatexBraceClose, skipOne, "]"));
                        }
                        break;
                    }
                    case '\\': {
                        push(str.tok(OTkDoubleSlash, skipOne, R"(\\)"));
                        break;
                    }
                    default: {
                        if (str.at(OMarkupChars, 1)) {
                            push(str.tok(OTkEscaped, skipCount, 1));
                        } else if (str.at(
                                       charsets::IdentStartChars
                                           - CharSet{'_'},
                                       1)) {
                            push(str.tok(OTkSymbolStart, skipOne, '\\'));
                            push(str.tok(
                                OTkIdent,
                                skipWhile,
                                charsets::IdentChars));
                            if (str.at('[')) {
                                push(str.tok(
                                    OTkMetaBraceOpen, skipOne, '['));
                                push(str.tok(
                                    OTkMetaBraceBody, [](PosStr& str) {
                                        skipBalancedSlice(
                                            str,
                                            {.openChars    = {'['},
                                             .closeChars   = {']'},
                                             .skippedStart = true,
                                             .consumeLast  = false});
                                    }));
                                push(str.tok(
                                    OTkMetaBraceClose, skipOne, ']'));
                            }
                            while (str.at('{')) {
                                push(str.tok(
                                    OTkMetaArgsOpen, skipOne, '{'));
                                push(str.tok(
                                    OTkMetaBraceBody, [](PosStr& str) {
                                        skipBalancedSlice(
                                            str,
                                            {.openChars    = {'{'},
                                             .closeChars   = {'}'},
                                             .skippedStart = true,
                                             .consumeLast  = false});
                                    }));

                                push(str.tok(
                                    OTkMetaArgsClose, skipOne, '}'));
                            }
                            break;
                        } else {
                            push(str.tok(OTkEscaped, skipCount, 2));
                        }
                    }
                };
                break;
            }
            case '~':
            case '`':
            case '=': {
                const auto start = str.get();
                if (str.at(start, 1)) {
                    push(str.tok(
                        markupConfig[start].inlineKind, skipCount, 2));
                    push(str.tok(OTkRawText, [start](PosStr& str) {
                        while (!str.at(start, start)) {
                            str.next();
                        }
                    }));
                    push(str.tok(
                        markupConfig[start].inlineKind, skipCount, 2));
                } else {
                    if (str.at(NonText, -1) || str.atStart()) {
                        push(str.tok(
                            markupConfig[start].startKind, skipCount, 1));
                        push(str.tok(OTkRawText, skipTo, start));
                        if (str.at(NonText, 1) || str.beforeEnd()) {
                            push(str.tok(
                                markupConfig[start].finishKind,
                                skipCount,
                                1));
                        }
                    } else {
                        push(str.tok(OTkPunctuation, skipCount, 1));
                    }
                }
                break;
            }
            case '<': {
                try {
                    lexAngle(str);
                    // REFACTOR remove exception for control handling, make
                    // interface more explicit
                } catch (UnexpectedCharError&) {
                    push(str.tok(OTkPunctuation, skipCount, 1));
                }
                break;
            }

            case '[': {
                lexBracket(str);
                break;
            }
            case '(': {
                push(str.tok(OTkParOpen, skipCount, 1));
                break;
            }
            case ')': {
                push(str.tok(OTkParClose, skipCount, 1));
                break;
            }
            case ':': {
                push(str.tok(OTkColon, skipCount, 1));
                break;
            }
            case '\'':
            case '?':
            case '!':
            case '%':
            case ']':
            case '|':
            case '&':
            case ';':
            case '}':
            case '>': {
                push(str.tok(OTkPunctuation, skipCount, 1));
                break;
            }
            case '{': {
                if (str.at("{{{")) {
                    push(str.tok(OTkMacroOpen, skipCount, 3));
                    push(str.tok(OTkIdent, [](PosStr& str) {
                        while (!str.finished() && !str.at('(')
                               && !str.at("}}}")) {
                            str.next();
                        }
                    }));

                    if (str.at('(')) {
                        lexParenArguments(str);
                    }
                    if (!str.finished()) {
                        push(str.tok(OTkMacroOpen, skipOne, "}}}"));
                    }
                } else {
                    push(str.tok(OTkMaybeWord, skipCount, 1));
                }
                break;
            }
            case '^': {
                push(str.tok(OTkCircumflex, skipCount, 1));
                break;
            }
            default: {
                if (str.at(charsets::TextChars)) {
                    lexTextChars(str);
                } else if (str.at(
                               markupKeys - CharSet{'<', '~', '`', '='})) {
                    const auto ch                        = str.get();
                    const auto& [kOpen, kClose, kInline] = markupConfig
                        [ch];
                    if (str.at((+(1)), ch)) {
                        push(str.tok(kInline, skipCount, 2));
                    } else if (str.at(NonText, -1) || str.atStart()) {
                        push(str.tok(kOpen, skipCount, 1));
                    } else if (str.at(NonText, 1) || str.beforeEnd()) {
                        push(str.tok(kClose, skipCount, 1));
                    } else {
                        push(str.tok(OTkWord, skipCount, 1));
                    }
                    break;
                } else {
                    throw str.makeUnexpected("any text character", "text");
                }
            }
        }
    }
};


int main(int argc, const char** argv) {
    return Catch::Session().run(argc, argv);
}
