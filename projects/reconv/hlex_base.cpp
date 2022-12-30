#include <utility>
#include <vector>
#include <string>
#include <climits>


namespace hax {

template <typename S, typename E>
struct HSlice {
    S start;
    E end;
};

template <typename T>
using Slice = HSlice<T, T>;

template <typename S, typename E>
HSlice<S, E> slice(S start, E end) {
    return {.start = start, .end = end};
}
} // namespace hax

using Str = std::string;

struct BackwardsIndex {
    int idx;
};

BackwardsIndex backIdx(const int& idx) { return {idx}; }


template <typename T>
struct Vec : public std::vector<T> {
    using std::vector<T>::operator[];
    using std::vector<T>::push_back;
    T& operator[](BackwardsIndex idx) {
        return (*this)[this->size() - idx.idx];
    }

    T& at(BackwardsIndex idx) { return this->at(this->size() - idx.idx); }

    void push_back(const Vec<T>& other) {
        for (const auto& item : other) {
            push_back(item);
        }
    }

    using std::vector<T>::back;
    using std::vector<T>::pop_back;
    T pop_back_v() {
        auto result = back();
        pop_back();
        return result;
    }

    using std::vector<T>::size;
    int high() const { return size() - 1; }
};

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
    union {
        struct {
            Vec<std::tuple<int, int, int>> ranges; /*!Sequence of
                                         starting position for ranges. When
                                         `popRange()` is called end
                                         position of the string (with
                                         offset) is used to determine end
                                         point.
                                         */
        };
        struct {
            int sliceIdx;            /*!Currently active slice
                                      */
            Vec<PosStrSlice> slices; /*!List of slices in the base
                                      * string
                                      */
            Vec<Vec<PosStrSlice>> fragmentedRanges; /*!Sequence
                  of fragments for active ranges. When `popRange()` is
                  called end position is used, identically to the
                  [[code:.ranges]] case. When position is advanced in
                  string, new fragments might be added to the ranges.
                  */
        };
    };
    int pos;    /*!Current absolute position in the base/buffer string.
  Always points to the current valid character. Calling
  [[code:advance()]] changes this position, potentially by
  an unlimited amount in case of fragmented string.
  */
    int line;   /*!Current line index. Automatically tracked by
  [[code:advance()]]
  */
    int column; /*!Current column number
                 */
    bool                  bufferActive;
    Vec<Vec<PosStrSlice>> sliceBuffer; /*!Buffer for new
  positional slices. Used by [[code:startSlice()]] and
  [[code:finishSlice()]] to automatically collect new string slices
  */
};

struct ParseError : public std::exception {};

struct HLexerError : public ParseError {
    int pos;
};

struct UnexpectedCharError : public HLexerError {};

struct UnbalancedWrapError : public HLexerError {};

struct MalformedTokenError : public HLexerError {};

/*!Get current line and column as tuple
 */
LineCol lineCol(const PosStr& str) { return {str.line, str.column}; }

/*!Get number of byte characters between end and start
 */
int len(const PosStrSlice& slice) {
    return ((slice.finish) - (slice.start));
}

char atAbsolute(const PosStr& str, const int& pos) {
    return (*(str.baseStr))[pos];
}

void decEnd(PosStr& str, const int& step = 1) {
    str.slices[backIdx(1)].finish -= step;
}

void incEnd(PosStr&& str, const int& step = 1) {
    ((str.slices[backIdx(1)].finish) += (step));
}

int absEnd(PosStr& str) { return str.slices[backIdx(1)].finish; }

/*! Get absolute position of the @arg{offset} */
int toAbsolute(const PosStrSlice& slice, const int& offset) {
    return ((slice.start) + (offset));
};

/*! Create new string with full buffer and `nil` input stream */
PosStr initPosStr(const Str& str, const LineCol& pos = {0, 0}) {
    PosStr result;
    result.baseStr = &str;
    result.isSlice = false;
    result.column  = pos.column;
    result.line    = pos.line;
    return result;
}

/*!Pop one layer of slices from slice buffer and create new sub-string
lexer from it.
*/
PosStr initPosStr(
    PosStr&     str,
    const bool& allSlice = false,
    const bool& popSlice = true) {
    PosStr           result;
    Vec<PosStrSlice> s;
    if (allSlice) {
        for (const auto slice : str.sliceBuffer) {
            s.push_back(slice);
        }
        if (popSlice) {
            str.sliceBuffer.clear();
        }
    } else {
        if (popSlice) {
            s.push_back(str.sliceBuffer.pop_back_v());
        } else {
            s.push_back(str.sliceBuffer.back());
        }
    };
    result.isSlice = true;
    result.baseStr = str.baseStr;
    result.column  = s[0].column;
    result.line    = s[0].line;
    result.slices  = s;
    result.pos     = result.slices[0].start;
    return result;
};

/*!Initl slice positional string, using @arg{inStr} as base
 */
PosStr initPosStr(const Str& inStr, const Vec<hax::Slice<int>>& slices) {
    PosStr result;
    result.isSlice = true;
    result.baseStr = &inStr;
    result.column  = 0;
    result.line    = 0;
    for (const auto slice : slices) {
        result.slices.push_back(PosStrSlice{
            .start  = slice.start,
            .finish = slice.end,
        });
    }
    result.pos = result.slices[0].start;
    return result;
}

/*!Create substring with `0 .. high(int)` slice range
 */
PosStr initPosStrView(const auto& str) {
    PosStr result;
    result.isSlice = true;
    result.baseStr = str.baseStr;
    result.column  = str.column;
    result.line    = str.line;
    // result.slices  = Vec<PosStrSlice>{
    //      PosStrSlice{.start = 0, .finish = INT_MAX}};
    return result;
}

bool contains(const PosStrSlice& slice, const int& position) {
    /*!Absolute position is within @arg{slice} start and end
     */
    return (
        (((slice.start) <= (position)))
        && (((position) <= (slice.finish))));
}

/*!Check if input string has at least @arg{idx} more characters left.
 */
bool hasNxt(const PosStr& input, const int& idx) {
    bool result;

    const auto pos = ((input.pos) + (idx));
    if (input.isSlice) {
        result
            = ((((input.sliceIdx) < (input.slices.high())))
               || ((
                   ((((((((input.sliceIdx) < (input.slices.size())))
                        && ((
                            (pos)
                            <= (input.slices[input.sliceIdx].finish)))))
                      && (((0) <= (pos)))))
                    && (((pos) < ((*(input.baseStr)).size())))))));
        ;
    } else {
        return (
            (((notNil(input.baseStr)) && (((0) <= (pos)))))
            && (((pos) < ((*(input.baseStr)).size()))));
        ;
    };
    return result;
}

/*!Check if string as no more input data
 */
bool finished(const PosStr& str) { return !(hasNxt(str, 0)); }
