#include <span>
#include <iostream>
#include <bitset>
#include <vector>
#include <optional>
#include <utility>
#include <memory>
#include <functional>

#include <catch2/catch_session.hpp>
#include <catch2/catch_test_macros.hpp>

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

    T& operator[](BackwardsIndex idx) {
        return (*this)[this->size() - idx.value];
    }

    T& at(BackwardsIndex idx) {
        return this->at(this->size() - idx.value);
    }

    void push_back(const Vec<T>& other) {
        for (const auto& item : other) {
            push_back(item);
        }
    }

    T pop_back_v() {
        auto result = back();
        pop_back();
        return result;
    }

    int high() const { return size() - 1; }

    int indexOf(CR<T> item) const {
        auto pos = std::find(begin(), end(), item);
        if (pos != end()) {
            return std::distance(pos, begin());
        } else {
            return -1;
        }
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

    void buildSet(R<IntSet<T>> result) {}

    template <typename ValueT>
    void buildSet(R<IntSet<T>> result, CR<ValueT> value) requires
        ConvertibleToSet<T, ValueT> {
        result.incl(value);
    }

    // Helper method to recurse into constructor argument list
    template <typename Value, typename... Args>
    void buildSet(
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

    bool contains(CR<T> value) const { return values.test(toIdx(value)); }
    /// Check if one set is a proper subset of another -- \arg other
    /// contains all the values.
    bool contains(CR<IntSet<T>> other) const {
        return ((values & other.values) == other.values);
    }


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

    int  size() const { return values.count(); }
    bool empty() const { return size() == 0; }

    /// Variadic constructor for the set type. It accepts only types that
    /// can be directly converted to the set
    template <typename... Args>
    IntSet(Args&&... args) requires AllConvertibleToSet<T, Args...> {
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

struct InvalidFormatString : public std::exception {};

/*!Iterate over interpolation fragments of the `formatstr`
 */
Vec<AddfFragment> addfFragments(const std::string& formatstr) {
    Vec<AddfFragment>  result{};
    auto               i   = 0;
    auto               num = 0;
    const IntSet<char> PatternChars{
        slice('a', 'z'),
        slice('A', 'Z'),
        slice('0', '9'),
        slice('\xF0', '\xFF'),
        '_'};


    /* TODO CONST SECTION */;
    while (i < formatstr.size()) {
        AddfFragment frag;
        if (((((formatstr[i]) == ('$')))
             && (((((i) + (1))) < ((formatstr.size())))))) {
            const auto c = formatstr[i + 1];
            if (c == '#') {
                frag = AddfFragment{
                    .kind = addfIndexed,
                    .idx  = num,
                };
                i += 2;
                num += 1;
            } else if (c == '$') {
                i += 2;
                frag = AddfFragment{
                    .kind = addfDollar,
                };

            } else if (charsets::Digits.contains(c) || c == '|') {
                auto j = 0;
                i += 1;
                const auto starti   = i;
                auto       negative = ((formatstr[i]) == ('-'));
                if (negative) {
                    i += 1;
                }
                while (
                    ((((i) < (formatstr.size())))
                     && (((charsets::Digits.contains(formatstr[i])))))) {
                    j = ((((((j) * (10))) + (ord(formatstr[i])))) - (ord('0')));
                    i += 1;
                }
                if (negative) {
                    frag = {.kind = addfBackIndexed, .idx = j};
                } else {
                    frag = AddfFragment{
                        .kind = addfIndexed,
                        .idx  = ((j) - (1)),
                    };
                }
            } else if (c == '{') {
                auto       j        = ((i) + (2));
                auto       k        = 0;
                auto       negative = ((formatstr[j]) == ('-'));
                const auto starti   = j;
                if (negative) {
                    j += 1;
                }
                auto isNumber = 0;
                while (
                    ((((j) < (formatstr.size())))
                     && ((!IntSet<char>({'\0', '}'})
                               .contains(formatstr[j]))))) {
                    if (((charsets::Digits.contains(formatstr[j])))) {
                        k = ((((((k) * (10))) + (ord(formatstr[j])))) - (ord('0')));
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
                        frag = AddfFragment{
                            .kind = addfBackIndexed,
                            .idx  = k,
                        };
                    } else {
                        frag = AddfFragment{
                            .kind = addfIndexed,
                            .idx  = ((k) - (1)),
                        };
                    };
                } else {
                    const auto first = i + 2;
                    const auto last  = j - 1;

                    frag = {
                        .kind = addfExpr,
                        .text = formatstr.substr(first, last - first + 1)};
                };
                i = ((j) + (1));
                break;

            } else if (
                charsets::Letters.contains(c) || c == '_'
                || IntSet<char>(slice('\xF0', '\xFF')).contains(c)) {
                auto j = ((i) + (1));
                while (
                    ((((j) < (formatstr.size())))
                     && (((PatternChars.contains(formatstr[j])))))) {
                    j += 1;
                };
                const auto first = i + 1;
                const auto last  = j - 1;

                frag = {
                    .kind = addfVar,
                    .text = formatstr.substr(first, last - first + 1)};
                i = j;
            } else {
                throw std::runtime_error(
                    R"(unexpected char after $ - )"
                    + std::string(1, formatstr[i + 1]));
            }
        } else {
            auto trange = slice(i, i);
            while (
                ((((trange.last) < (formatstr.size())))
                 && (((formatstr[trange.last]) != ('$'))))) {
                trange.last += 1;
            };
            trange.last -= 1;
            frag = {
                .kind = addfText,
                .text = formatstr.substr(
                    trange.first, trange.last - trange.first + 1),
            };
            i = trange.last;
            i += 1;
        }
        result.push_back(frag);
    };
    return result;
}

/*! The same as `add(s, formatstr % a)`, but more efficient. */
void addf(
    std::string&            s,
    const std::string&      formatstr,
    const Vec<std::string>& a) {
    const auto fragments = addfFragments(formatstr);
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
                    throw std::runtime_error(
                        "Argument index out of bounds. Accessed "
                        + std::to_string(idx) + ", but only "
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
                auto x = a.indexOf(fr.text);
                if ((0 <= x) && (x < a.high())) {
                    s += a[((x) + (1))];
                } else {
                    throw std::runtime_error(
                        "No interpolation argument named " + fr.text);
                };
                break;
            }
        };
    };
}


void addf(
    std::string&                               s,
    const std::string&                         formatstr,
    const Vec<Pair<std::string, std::string>>& a) {
    Vec<std::string> tmp;
    for (const auto& [key, val] : a) {
        tmp.push_back(key);
        tmp.push_back(val);
    }
    addf(s, formatstr, tmp);
}

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


enum class SomeEnum : unsigned short
{
    FirstValue    = 0,
    SecondValue   = 1,
    RangeStart    = 2,
    RangeElement1 = 3,
    RangeElement2 = 4,
    RangeEnd      = 5
};

struct ParseError : public std::exception {
    int line;
    int column;
};
struct LexerError : public ParseError {
    int pos;
};
struct UnexpectedCharError : public LexerError {};
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

struct LineCol {
    int line;
    int column;
};

struct PosStrSlice {
    int line;   /*! Slice start line */
    int column; /*! Slice start column */
    int start;  /*! Start byte */
    int finish; /*! End byte */
};

struct PosStr {
    PosStr() {}
    ~PosStr() {}
    PosStr(const PosStr& other) {}
    const std::string* baseStr; /*!For non-slice string used as buffer.
  Might contain full input data (in case of lexing over existing string).
  For slice string contains reference to the original string (is not
  modified)
  */
    bool isSlice;
    int  pos; /*!Current absolute position in the base/buffer string.
Always points to the current valid character. Calling
[[code:advance()]] changes this position, potentially by
an unlimited amount in case of fragmented string.
*/
    int line; /// Current line index. Automatically tracked by
              /// [[code:advance()]]
    int                   column; /// Current column number
    bool                  bufferActive;
    Vec<Vec<PosStrSlice>> sliceBuffer;
    /// Buffer for new positional
    /// slices. Used by [[code:startSlice()]] and [[code:finishSlice()]] to
    /// automatically collect new string slices
};


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


int main(int argc, const char** argv) {
    {
        std::string res;
        addf(res, "[$1]??", {Str("value").leftAligned(20)});
        std::cout << res << "\n";
    }

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
        std::cout << "[" << c << "] ";
    }
    std::cout << view.size() << "\n";
    int result = Catch::Session().run(argc, argv);
}
