#include <utility>
#include <vector>
#include <string>


using Str = std::string;

struct BackwardsIndex
{
    int idx;
};

BackwardsIndex backIdx(const int& idx) { return {idx}; }


template <typename T>
struct Vec : public std::vector<T>
{
    using std::vector<T>::operator[];

    T& operator[](BackwardsIndex idx) {
        return (*this)[this->size() - idx.idx];
    }

    T& at(BackwardsIndex idx) { return this->at(this->size() - idx.idx); }
};


struct LineCol
{
    int line;
    int column;
};

struct PosStrSlice
{
    int line;   /*! Slice start line */
    int column; /*! Slice start column */
    int start;  /*! Start byte */
    int finish; /*! End byte */
};


struct PosStr
{
    std::string* baseStr; /*!For non-slice string used as buffer.
  Might contain full input data (in case of lexing over existing string).
  For slice string contains reference to the original string (is not
  modified)
  */
    bool isSlice;
    union {
        struct
        {
            Vec<std::tuple<int, int, int>> ranges; /*!Sequence of
                                         starting position for ranges. When
                                         `popRange()` is called end
                                         position of the string (with
                                         offset) is used to determine end
                                         point.
                                         */
        };
        struct
        {
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

struct ParseError : public std::exception
{
};

struct HLexerError : public ParseError
{
    int pos;
};

struct UnexpectedCharError : public HLexerError
{
};

struct UnbalancedWrapError : public HLexerError
{
};

struct MalformedTokenError : public HLexerError
{
};

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
