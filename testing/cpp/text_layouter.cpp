#include <vector>
#include <optional>

const auto infty = 1024 * 1024 * 1024; // "Very large value"

/*!Check if value is 'very large'
 */
bool inf(const int& a) {
    return ((infty - 4096) <= a) && (a <= (infty + 4096));
};

template <typename T>
std::vector<T> get(const std::vector<std::optional<T>>& inseq) {
    std::vector<T> result;
    for (const auto elem : inseq) {
        if (elem.isSome()) {
            result.add(elem.get());
        };
    };
    return result;
};

template <typename T1, typename T2, typename T3, typename T4, typename T5>
std::vector<std::tuple<T1, T2, T3, T4, T5>> zip(
    const std::vector<T1>& s1,
    const std::vector<T2>& s2,
    const std::vector<T3>& s3,
    const std::vector<T4>& s4,
    const std::vector<T5>& s5) {
    std::vector<std::tuple<T1, T2, T3, T4, T5>> result{};
    for (int idx = 0; idx < min({s1.len, s2.len, s3.len, s4.len, s5.len});
         ++idx) {
        result.push_back({
            s1[idx],
            s2[idx],
            s3[idx],
            s4[idx],
            s5[idx],
        });
    };
    return result;
};

template <typename T>
std::vector<std::tuple<int, T&>> rmpairs(std::vector<T>& s) {
    std::vector<std::tuple<int, T&>> result{};
    /*!Iterate over mutable sequence starting from the right
     */

    for (const auto idx : countdown(((s.len) - (1)), 0)) {
        result.push_back({
            idx,
            s[idx],
        });
    };
    return result;
};

enum LayoutElementKind
{
    lekString,
    lekNewline,
    lekNewlineSpace,
    lekLayoutPrint,
};

struct LayoutElement {
    int               id;
    LayoutElementKind kind;
    union { /* IF lekString */
        struct {
            LytStrSpan /* PRAGMA  {.requiresInit.} */ text; /*!Layout
                                                             * element text
                                                             */
        }; /* IF lekNewlineSpace */
        struct {
            int spaceNum;
        }; /* IF lekLayoutPrint */
        struct {
            Layout layout;
        };       /* IF lekNewline */
        struct { /* NIL LIT */
        };
    };
    bool indent;
};


enum LytEventKind
{
    layEvStr,
    layEvNewline,
    layEvSpaces,
};

struct LytEvent {
    LytEventKind kind;
    union { /* IF layEvStr */
        struct {
            LytStr str;
        }; /* IF layEvSpaces */
        struct {
            int spaces;
        };       /* IF layEvNewline */
        struct { /* NIL LIT */
        };
    };
};

struct Layout {
    std::vector<LayoutElement> elements;
};

struct LytSolution {
    int                id;
    std::vector<int>   knots;
    std::vector<int>   spans;
    std::vector<float> intercepts; /*!constant cost associated with each
knot - computed cost of the outputting this layout solution at each know in
the `knots` list
*/
    std::vector<float> gradients;  /*!at each knot, the rate with which the
 layout  cost increases with an additional margin indent of 1 character.
*/
    std::vector<Layout> layouts;   /*!the Layout objects expressing the
  optimal   layout between each knot.
 */
    int index;
};


enum LytBlockKind
{
    bkText,
    /*!A layout consisting of a single line of unbroken text.
     */
    bkLine,
    /*!Horizontally stacked lines
     */
    bkChoice,
    /*!Several alternating layouts
     */
    bkStack,
    /*!Vertically stacked layouts
     */
    bkWrap,
    /*!Mulitple blocks wrapped to create lowerst-cost layout
     */
    bkVerb,
    /*!Multiple lines verbatim
     */
    bkEmpty,
    /*!Empty layout block - ignored by `add` etc.
     */
};
/* TYPE DEF LytStr* = object
  id*: LytStrId ## Single layout string object. It contains all the information
                ## required to perform the layout and refer back to the original string
                ## piece if needed.
  ## Id of the original piece of text
  len*: int ## It's lengh in units (units are specified - can be ASCII or
            ## unicode or anything else)
   */
struct LytStr {
    LytStrId id; /*!Id of the original piece of text
                  */
    int len; /*!It's lengh in units (units are specified - can be ASCII or
unicode or anything else)
*/
};

/* TYPE DEF LytStrSpan* = object
  strs: seq[LytStr]          ## Span of multiple layout strings
  len: int
 */
struct LytStrSpan {
    std::vector<LytStr> strs;
    int                 len;
};


/* FIXME REF OBJ */ struct LytBlock {
    Table<std::optional<LytSolution>, std::optional<LytSolution>>
                                        layoutCache;
    bool /* PRAGMA  {.requiresInit.} */ isBreaking; /*!Whether or not this
                                                     * block should end the
                                                     * line
                                                     */
    int /* PRAGMA  {.requiresInit.} */ breakMult; /*!Local line break cost
                                                   * change
                                                   */
    int /* PRAGMA  {.requiresInit.} */ id;
    LytBlockKind                       kind;
    union { /* IF bkVerb */
        struct {
            std::vector<LytStrSpan> textLines; /*!Multiple lines of text
                                                */
            bool firstNl; /*!Insert newline at the block start
                           */
        };                /* IF bkText */
        struct {
            LytStrSpan text; /*!A single line of text, free of carriage
returs etc.
*/
        };                   /* IF bkWrap */
        struct {
            std::optional<LytStr> prefix;
            LytStr                sep; /*!Separator for block wraps
                                        */
            std::vector<LytBlock> wrapElements;
        }; /* IF bkStack, bkChoice, bkLine */
        struct {
            std::vector<LytBlock> elements;
        };       /* IF bkEmpty */
        struct { /* NIL LIT */
        };
    };
};

/* TYPE DEF LytFormatPolicy = object
  breakElementLines: proc (blc: seq[seq[LytBlock]]): seq[seq[LytBlock]] ## Hook
   */
struct LytFormatPolicy { std::function<std::vector<Vec<LytBlock>>(std::vector<Vec<LytBlock>> breakElementLines  ; /*!Hook
*/
};


struct LytOptions {
    int leftMargin;  /*!position of the first right margin. Expected `0`
                      */
    int rightMargin; /*!position of the second right margin. Set for `80`
to wrap on default column limit.
*/
    float leftMarginCost;  /*!cost (per character) beyond margin 0.
Expected value `~0.05`
*/
    float rightMarginCost; /*!cost (per character) beyond margin 1. Should
be much higher than `c0`. Expected value
`~100`
*/
    int linebreakCost;     /*!cost per line-break
                            */
    int indentSpaces;      /*!spaces per indent
                            */
    float cpack; /*!cost (per element) for packing justified layouts.
Expected value `~0.001`
*/
    LytFormatPolicy formatPolicy;
};

/* TYPE DEF OutConsole* = object
  leftMargin: int
  rightMargin: int
  hPos: int                  ## Horizontal position on the output console
  margins: seq[int]
 */
struct OutConsole {
    int leftMargin;
    int rightMargin;
    int hPos; /*!Horizontal position on the output console
               */
    std::vector<int> margins;
};


string operator $(const LytStr& s) {
    string result;
    if (((s.id) == (LytSpacesId))) {
        (&(R"({s.len} spaces)"));
    } else {
        toStr(s.id);
    };
    return result;
};

bool isSpaces(const LytStr& s) {
    bool result;
    ((s.id) == (LytSpacesId));
    return result;
};

bool isEmpty(const LytStr& s) {
    bool result;
    ((s.id.int) == (0));
    return result;
};

LytStr lytStrSpaces(const int& spaces) {
    LytStr result;
    LytStr{
        .id  = LytSpacesId,
        .len = spaces,
    };
    return result;
};

LytStr lytStrIdx(const int& idx, const int& len) {
    LytStr result;
    /*!Create layout string object from the element index
     */

    LytStr{
        .id  = toLytStrId(idx),
        .len = len,
    };
    return result;
};

LytStrSpan lytStrSpan(const LytStr& str) {
    LytStrSpan result;
    /*!Construct layout string span with single element
     */

    result.strs = (@({str}));

    result.len = str.len;

    return result;
};

LytStrSpan lytStrSpan(const openArray<LytStr>& strs) {
    LytStrSpan result;
    /*!Construct layout string span from multiple elements
     */

    result.strs = (@(strs));

    for (const auto str : strs) {
        ((result.len) += (str.len));
    };
    return result;
};

int margin(const OutConsole& buf) {
    int result;
    buf.margins[backIdx(1)];
    return result;
};

void addMargin(OutConsole& c, const int& m) { c.margins.add(m); };

void popMargin(OutConsole& c) { c.margins.pop; };

LytEvent event(const LytStr& s) {
    LytEvent result;
    LytEvent{
        .kind = layEvStr,
        .str  = s,
    };
    return result;
};

LytEvent event(const int& spaces) {
    LytEvent result;
    LytEvent{
        .kind   = layEvSpaces,
        .spaces = spaces,
    };
    return result;
};

LytEvent event() {
    LytEvent result;
    LytEvent{
        .kind = layEvNewline,
    };
    return result;
};

string format( const LytStr& str   ,  const std::function<Str(LytStr& getStr   ) {
    string result;
    if (getStr.isNil()) {
        result.add(toStr(str));
    } else {
        result.add(getStr(str));
    };
    return result;};

string format( const LytStrSpan& span   ,  const std::function<Str(LytStr& getStr   ) {
    string result;
    result.add(R"([)");
    for (const auto [idx, item] : span.strs) {
        if (((0) < (idx))) {
            result.add(R"(, )");
        };
        result.add(format(item, getStr));
    };
    result.add(R"(])");
    return result;};

string treeRepr( const Layout& self   ,  const std::function<Str(LytStr& getStr  = /* NIL LIT */  ,  const int& level  = 0  ) {
    string result;

    auto r = addr(result);


    void add(const Str& s) { (*(r)).add(s); };

    void aux(const Layout& lyt, const int& l){};

    void aux(const LayoutElement& lyt, const int& l) {
        add(repeat(R"(  )", l));
        add((&(R"(id {lyt.id} )")));
        switch (lyt.kind) {
            case lekString: {
                add(R"([text] 《)");
                add(lyt.text.format(getStr));
                add(R"(》\n)");
                break;
            }
            case lekNewline: {
                add(R"([newline]\n)");
                break;
            }
            case lekNewlineSpace: {
                add(R"([newline][space]\n)");
                break;
            }
            case lekLayoutPrint: {
                add(R"([lyt]\n)");
                aux(lyt.layout, ((l) + (1)));
                break;
            }
        };
    };

    void aux(const Layout& lyt, const int& l) {
        for (const auto [idx, elem] : pairs(lyt.elements)) {
            if ((((!(idx))) == (0))) {
                add(R"(\n)");
            };
            aux(elem, l);
        };
    };
    aux(self, level);
    return result;};

string treeRepr( const LytSolution& self   ,  const std::function<Str(LytStr& getStr  = /* NIL LIT */  ,  const int& level  = 0  ) {
    string result;
    result.add(R"([lyt solution]\n)");
    for (const auto lyt : self.layouts) {
        result.add(R"(  [lyt]\n)");
        result.add(treeRepr(lyt, getStr, 2));
        result.add(R"(\n)");
    };
    return result;};

string operator $( const LayoutElement& le   ) {
    string result;
    toStr(le.text);
    return result;};

string operator $( const LytSolution& sln   ) {
    string result;
    ((result) &= (R"(<)"));

    int idx = 0;


    for (const auto s :
         zip(sln.knots,
             sln.spans,
             sln.intercepts,
             sln.gradients,
             sln.layouts)) {
        if (((idx) > (0))) {
            ((result) &= (R"(, )"));
        };
        ((result) &= ((&(R"({s[0]}/({s[1]}, {s[2]:.2}, {s[3]}))"))));
        += idx;
    };
    ((result) &= (R"(>)"));
    return result;};

string operator $( const std::optional<LytSolution>& sln   ) {
    string result;
    if (sln.isSome()) {
        return toStr(sln.get());
    };
    return result;};

string operator $( const LytBlock& blc   ) {
    string result;
((result) &= (/* FIXME STMT LIST EXPR */
switch (blc.kind) {case bkText:{
((((R"(T[)") & (/* FIXME STMT LIST EXPR */
else {
        blc.isBreaking }else { R"()" }))) & ((&(R"("{blc.text}"])"))));break;}case bkStack:{
    ((((R"(V[)") & (blc.elements.mapIt(toStr(it)).join(R"( ↕ )"))))
     & (R"(])"));
    break;}case bkLine:{
    ((((R"(H[)") & (blc.elements.mapIt(toStr(it)).join(R"( ↔ )"))))
     & (R"(])"));
    break;}case bkChoice:{
    ((((R"(()") & (blc.elements.mapIt(toStr(it)).join(R"( ? )"))))
     & (R"())"));
    break;}case bkWrap:{
    ((((R"([)") & (blc.wrapElements.mapIt(toStr(it)).join(R"( )"))))
     & (R"(])"));
    break;}case bkVerb:{
    ((toStr(blc.textLines[0].strs[0].id.int)) & (R"(...)"));
    break;}case bkEmpty:{
    R"(<empty>)";
    break;}
}));
return result;
}


string treeRepr( const LytBlock& inBl   ,  const std::function<Str(LytStr& getStr  = /* NIL LIT */  ) {
    string result;

    string aux(const LytBlock& bl, const int& level) {
        string result;

        const auto name = switch (bl.kind){case bkLine : {R"(L)";
        break;
    }
    case bkChoice: {
        R"(C)";
        break;
    }
    case bkText: {
        R"(T)";
        break;
    }
    case bkWrap: {
        R"(W)";
        break;
    }
    case bkStack: {
        R"(S)";
        break;
    }
    case bkVerb: {
        R"(V)";
        break;
    }
    case bkEmpty: {
        R"(E)";
        break;
    }} ; ;;

 auto pref  = align(((name) & (R"( )")), ((level) * (2))) ; ;;
  if (bl.isBreaking) {
    pref.add((&(R"(brk: {bl.isBreaking} )")));  };
  if (((bl.breakMult) != (1))) {
    pref.add((&(R"(mul: {bl.breakMult} )")));  };

const auto pref2  = repeat(R"( )", ((level) * (2))) ; ;;
result = ((pref2) & (pref));;
switch (bl.kind) {
    case bkLine:
    case bkChoice:
    case bkStack:
    case bkWrap: {
        ((result) &= (R"(\n)"));
        for (const auto elem : items(
                 else { ((bl.kind) == (bkWrap)) } else { bl.elements; })) {
            ((result) &= (elem.aux(((level) + (1)))));
        };
        break;
    }
    case bkText: {
        ((result) &= ((
             (((R"(〈)") & (bl.text.format(getStr)))) & (R"(〉\n)"))));
        break;
    }
    case bkEmpty: {
        ((result) &= (R"(<empty>)"));
        break;
    }
    case bkVerb: {
        ((result) &= (R"(\n)"));
        for (const auto line : items(bl.textLines)) {
            ((result) &= ((
                 (((((((pref2)
                       & (repeat(
                           R"(  )",
                           clamp(((level) - (1)), 0, high(int))))))
                     & (R"(  〚)")))
                   & (line.format(getStr))))
                 & (R"(〛\n)"))));
        };
        break;
    }
    }

    return result;
}

return aux(inBl, 0);

return result;
}


Hash hash(const LayoutElement& elem) {
    Hash result;
    hash(elem.id);
    return result;
};

Hash hash(const Layout& lyt) {
    Hash result;
    hash(lyt.elements);
    return result;
};

Hash hash(const std::optional<LytSolution>& sln) {
    Hash result;
    if (sln.isNone()) {
        return;

    } else {
        return sln.get.id;
    };
    return result;
};

int getSId() {
    int result;

    int /* PRAGMA  {.global.} */ slnId;


    /* PRAGMA BLOCK  {.cast(noSideEffect).} */

    += slnId;
    result = slnId;


    return result;
};

LayoutElement lytString(const LytStrSpan& s) {
    LayoutElement result;
    LayoutElement{
        .text = s,
        .kind = lekString,
        .id   = getSId(),
    };
    return result;
};

LayoutElement lytNewline(const bool& indent = true) {
    LayoutElement result;
    LayoutElement{
        .indent = indent,
        .kind   = lekNewline,
        .id     = getSId(),
    };
    return result;
};

LayoutElement lytNewlineSpace(const int& n) {
    LayoutElement result;
    LayoutElement{
        .spaceNum = n,
        .kind     = lekNewlineSpace,
        .id       = getSId(),
    };
    return result;
};

LayoutElement lytPrint(const Layout& lyt) {
    LayoutElement result;
    LayoutElement{
        .kind   = lekLayoutPrint,
        .layout = lyt,
        .id     = getSId(),
    };
    return result;
};

Layout getStacked(const std::vector<Layout>& layouts) {
    Layout result;
    /*!Return the vertical composition of a sequence of layouts.
     */

    /*!Args:
      layouts: a sequence of Layout objects.
    Returns:
      A new Layout, stacking the arguments.
    */


    std::vector<LayoutElement> lElts;


    for (const auto l : layouts) {
        for (const auto e : l.elements) {
            lElts.add(e);
        };
        lElts.add(lytNewline());
    };
    return Layout{
        .elements = lElts[rangeIncl(0, backIdx(2))],
    };

    return result;
};

Layout initLayout(const std::vector<LayoutElement>& elems) {
    Layout result;
    Layout{
        .elements = elems,
    };
    return result;
};

LytSolution initSolution(
    const std::vector<int>&    knots,
    const std::vector<int>&    spans,
    const std::vector<float>&  intercepts,
    const std::vector<float>&  gradients,
    const std::vector<Layout>& layouts) {
    LytSolution result;
    result = LytSolution{
        .knots      = knots,
        .spans      = spans,
        .intercepts = intercepts,
        .gradients  = gradients,
        .layouts    = layouts,
        .id         = getSId(),
    };

    return result;
};

void reset(LytSolution& self) {
    /*!Begin iteration.
     */

    self.index = 0;
};

void advance(LytSolution& self) {
    /*!Advance to the next knot.
     */

    ((self.index) += (1));
};

void retreat(LytSolution& self) {
    /*!Move back a knot.
     */

    ((self.index) -= (1));
};

int curKnot(const LytSolution& self) {
    int result;
    /*!The currently indexed knot.
     */

    return self.knots[self.index];

    return result;
};

int curSpan(const LytSolution& self) {
    int result;
    return self.spans[self.index];

    return result;
};

float curIntercept(const LytSolution& self) {
    float result;
    return self.intercepts[self.index];

    return result;
};

float curGradient(const LytSolution& self) {
    float result;
    return self.gradients[self.index];

    return result;
};

Layout curLayout(const LytSolution& self) {
    Layout result;
    self.layouts[self.index];
    return result;
};

int curIndex(const LytSolution& self) {
    int result;
    self.index;
    return result;
};

float curValueAt(const LytSolution& self, const int& margin) {
    float result;
    /*!The value (cost) extrapolated for margin m from the current
     * knot.
     */

    return (
        (self.curIntercept())
        + (((self.curGradient())
            * (float(((margin) - (self.curKnot())))))));

    return result;
};

int nextKnot(const LytSolution& self) {
    int result;
    /*!The knot after the once currently indexed.
     */

    if (((((self.index) + (1))) >= (self.knots.len))) {
        infty;
    } else {
        self.knots[((self.index) + (1))];
    };
    return result;
};

void moveToMargin(LytSolution& self, const int& margin) {
    /*!Adjust the index so m falls between the current knot and the
     * next.
     */

    if (((self.curKnot()) > (margin))) {
        while (((self.curKnot()) > (margin))) {
            self.retreat();
        };
    } else {
        while (
            ((((self.nextKnot()) <= (margin)))
             && (((self.nextKnot()) != (infty))))) {
            self.advance();
        };
    };
};

void add(
    LytSolution&               self,
    const int& knot const int& span,
    ,
    const float& intercept const float& gradient,
    ,
    const Layout& layout) {
    /*!Add a segment to a LytSolution under construction.

    The function performs basic consistency checks, and eliminates
    redundant segments that are linear extrapolations of those that
    precede them.
    */

    if (self.isNil()) {
        new (self);
    };
    if (((0) < (self.knots.len))) {

        const auto kLast = self.knots[backIdx(1)];

        const auto sLast = self.spans[backIdx(1)];

        const auto iLast = self.intercepts[backIdx(1)];

        const auto gLast = self.gradients[backIdx(1)];


        if ((((((((span) == (sLast))) && (((gradient) == (gLast)))))
              && ((
                  (((iLast) + ((((((knot) - (kLast)))) * (gLast)))))
                  == (intercept)))))) {
            return;
        };
    };
    if (((((((((knot) < (0))) || (((span) < (0)))))
           || (((intercept) < (0)))))
         || (((gradient) < (0))))) {
        raiseAssert((
            (R"(Internal error: bad layout: )")
            & ((&(
                R"((k {knot}, s {span}, i {intercept}, g {gradient}))")))));
    };
    self.knots.add(knot);
    self.spans.add(span);
    self.intercepts.add(intercept);
    self.gradients.add(gradient);
    self.layouts.add(layout);
};

Option[LytSolution] minSolution(
    const std::vector<LytSolution>& solutions) {
    Option[LytSolution] result;
    /*!Form the piecewise minimum of a sequence of LytSolutions.

    Args:
      solutions: a non-empty sequence of LytSolution objects
    Returns:
      values LytSolution object whose cost is the piecewise minimum of
    the LytSolutions provided, and which associates the minimum-cost
    layout with each piece.
    */

    if (((len(solutions)) == (1))) {
        return some(solutions[0]);
    };

    LytSolution factory;

    auto solutions = solutions;


    new (factory);
    for (const auto s : mitems(solutions)) {
        s.reset();
    };

    const auto n = len(solutions);


    auto kL = 0;

    auto lastIMinSoln = -1;

    auto lastIndex = -1;


    while (((kL) < (infty))) {

        const auto kH = ((min(solutions.map(nextKnot))) - (1));

        const auto gradients = solutions.map(curGradient);


        while (true) {

            const auto values = solutions.mapIt(it.curValueAt(kL));


            auto [minValue, minGradient, iMinSoln, ](rangeExcl(0, n))
                .mapIt({
                    values[it],
                    gradients[it],
                    it,
                })
                .min();


            const auto minSoln = solutions[iMinSoln];


            if (((((iMinSoln) != (lastIMinSoln)))
                 || (((minSoln.curIndex()) != (lastIndex))))) {
                factory.add(
                    kL,
                    minSoln.curSpan(),
                    minValue,
                    minGradient,
                    minSoln.curLayout());
                lastIMinSoln = iMinSoln;

                lastIndex = minSoln.curIndex();
            };

            const auto distancesToCross = collect(
                newSeq, for (int i = 0; i < n; ++i) {
                    if (((gradients[i]) < (minGradient))) {
                        ceil(
                            (((((values[i]) - (minValue))))
                             / ((((minGradient) - (gradients[i]))))));
                    };
                };);


            const auto crossovers = collect(
                newSeq,
                for (const auto d
                     : distancesToCross) {
                    if (((((kL) + (d))) <= (kH))) {
                        ((kL) + (d));
                    };
                };);


            if (((crossovers.len) > (0))) {
                kL = min(crossovers).int;

            } else {
                kL = ((kH) + (1));

                if (((kL) < (infty))) {
                    for (const auto s : mitems(solutions)) {
                        s.moveToMargin(kL);
                    };
                };
                break;
            };
        };
    };
    return some(factory);

    return result;
};

LytSolution vSumSolution(const std::vector<LytSolution>& solutions) {
    LytSolution result;
    /*!The layout that results from stacking several LytSolutions
    vertically. Args: solutions: a non-empty sequence of LytSolution
    objects Returns: A LytSolution object that lays out the solutions
    vertically, separated by newlines, with the same left margin.
    */

    assert(((solutions.len) > (0)));
    if (((len(solutions)) == (1))) {
        return solutions[0];
    };

    auto solutions = solutions;


    for (const auto s : mitems(solutions)) {
        s.reset();
    };

    auto margin = 0;


    while (true) {
        result.add(
            margin,
            solutions[backIdx(1)].curSpan(),
            solutions.mapIt(it.curValueAt(margin)).sum(),
            solutions.mapIt(it.curGradient()).sum(),
            getStacked(solutions.mapIt(it.curLayout())));

        const auto dStar = min(
            solutions.filterIt(((it.nextKnot()) > (margin)))
                .mapIt(((it.nextKnot()) - (margin))));


        if (dStar.inf) {
            break;
        };
        ((margin) += (dStar));
        for (const auto s : mitems(solutions)) {
            s.moveToMargin(margin);
        };
    };
    return result;
};

LytSolution hPlusSolution(
    LytSolution& s1 LytSolution& s2,
    ,
    const LytOptions& opts) {
    LytSolution result;
    /*!The LytSolution that results from joining two LytSolutions
    side-by-side.

    Args:
      `s1`: LytSolution object
      `s2`: LytSolution object
    Returns:
      A new LytSolution reflecting a layout in which `s2` ('s layout)
    is placed immediately to the right of `s1`.

    The resulting LytSolution object maps each prospective left margin
    m to the span, cost and layout information that would result from
    siting LytSolution `s1` at m, and then placing `s2` at margin `m +
    sp1(m)`, where `sp1(m)` is the span of characters occupied by the
    layout to which `s1` maps m. In general, of course, both s1 and
    `s2`'s layouts may occupy multiple lines, in which case `s2`'s
    layout begins at the end of the last line of `s1`'s layout---the
    span in this case is the span of `s1`'s last line.
    */

    s1.reset();
    s2.reset();

    auto s1Margin = 0;

    auto s2Margin = s1.curSpan();


    s2.moveToMargin(s2Margin);
    while (true) {

        const auto g1 = s1.curGradient();

        const auto g2 = s2.curGradient();

        const auto overhang0 = ((s2Margin) - (opts.leftMargin));

        const auto overhang1 = ((s2Margin) - (opts.rightMargin));

        const auto gCur = ((
            (((((g1) + (g2)))
              - (/* FIXME STMT LIST EXPR */
                 else { ((overhang0) >= (0)) } else {0})))
            - (/* FIXME STMT LIST EXPR */
               else { ((overhang1) >= (0)) } else {0})));

        const auto iCur = ((
            (((((s1.curValueAt(s1Margin)) + (s2.curValueAt(s2Margin))))
              - (((opts.leftMarginCost) * (max(overhang0, 0))))))
            - (((opts.rightMarginCost) * (max(overhang1, 0))))));


        result.add(
            s1Margin,
            ((s1.curSpan()) + (s2.curSpan())),
            iCur,
            gCur,
            initLayout((
                @({lytPrint(s1.curLayout()), lytPrint(s2.curLayout())}))));

        const auto kn1 = s1.nextKnot();

        const auto kn2 = s2.nextKnot();


        if (((kn1.inf) && (kn2.inf))) {
            break;
        };
        if (((((kn1) - (s1Margin))) <= (((kn2) - (s2Margin))))) {
            s1.advance();
            s1Margin = kn1;

            s2Margin = ((s1Margin) + (s1.curSpan()));

            s2.moveToMargin(s2Margin);
        } else {
            s2.advance();
            s2Margin = kn2;

            s1Margin = ((s2Margin) - (s1.curSpan()));
        };
    };
    return result;
};

LytSolution plusConst(const LytSolution& self, const float& val) {
    LytSolution result;
    /*!Add a constant to all values of this LytSolution.
     */

    result = self;

    for (const auto a : mitems(result.intercepts)) {
        ((a) += (val));
    };
    return result;
};

Option[LytSolution] withRestOfLine(
    std::optional<LytSolution>& self,
    std::optional<LytSolution>& rest,
    const LytOptions&           opts) {
    Option[LytSolution] result;
    /*!Return a LytSolution that joins the rest of the line right of
    this one.

    Args:
      rest: a LytSolution object representing the code laid out on the
        remainder of the line, or None, if the rest of the line is
    empty. Returns: A new LytSolution object juxtaposing the layout
    represented by this LytSolution to the immediate right of the
    remainder of the line.
    */

    if (rest.isNone()) {
        self;
    } else {
        some(self.get().hPlusSolution(rest.get(), opts));
    };
    return result;
};

seq[LytBlock] elements(const LytBlock& self) {
    seq[LytBlock] result;
    if (contains({bkWrap}, self.kind)) {
        return self.elements;
    };
    if (contains({bkStack, bkChoice, bkLine}, self.kind)) {
        return self.elements;
    };
    raiseAssert(R"(#[ IMPLEMENT:ERRMSG ]#)");
    return result;
};

void operator elements =(LytBlock& self, const std::vector<LytBlock>& it) {

    bool matched = false;


    if (contains({bkWrap}, self.kind)) {
        if (true) {
            matched = true;

            self.wrapElements = it;
        };
    };
    if (contains({bkStack, bkChoice, bkLine}, self.kind)) {
        if (true) {
            matched = true;

            self.elements = it;
        };
    };
    if ((!(matched))) {
        raiseAssert(R"(#[ IMPLEMENT:ERRMSG ]#)");
    };
};

int len(const LytBlock& blc) {
    int result;
    switch (blc.kind) {
        case bkWrap: {
            blc.wrapElements.len();
            break;
        }
        case bkStack:
        case bkChoice:
        case bkLine: {
            blc.elements.len();
            break;
        }
        default: {
            0;
        }
    };
    return result;
};

LytBlock operator[](const LytBlock& blc, const int& idx) {
    LytBlock result;
    blc.elements[idx];
    return result;
};

LytBlock& operator[](LytBlock& blc, const int& idx) {
    LytBlock& result;
    blc.elements[idx];
    return result;
};

std::vector<LytBlock> items(const LytBlock& blc) {
    std::vector<LytBlock> result{};
    for (const auto item : blc.elements) {
        result.push_back(item);
    };
    return result;
};

std::vector<{
    int,
    LytBlock,
}> pairs(const LytBlock& blc) {
    std::vector<{
        int,
        LytBlock,
    }>
        result{};
    for (const auto [idx, item] : blc.elements) {
        result.push_back({
            idx,
            item,
        });
    };
    return result;
};

std::vector<LytBlock&> mitems(LytBlock& blc) {
    std::vector<LytBlock&> result{};
    for (const auto item : mitems(blc.elements)) {
        result.push_back(item);
    };
    return result;
};

std::vector<{
    int,
    LytBlock&,
}> mpairs(LytBlock& blc) {
    std::vector<{
        int,
        LytBlock&,
    }>
        result{};
    for (const auto [idx, item] : mpairs(blc.elements)) {
        result.push_back({
            idx,
            item,
        });
    };
    return result;
};

int getBId() {
    int result;

    int /* PRAGMA  {.global.} */ id;


    /* PRAGMA BLOCK  {.cast(noSideEffect).} */

    += id;
    return id;


    return result;
};

LytBlock initBlock(const LytBlockKind& kind, const int& breakMult = 1) {
    LytBlock result;
    assert(((kind)notin({bkText})));
    result = LytBlock{
        .id         = getBId(),
        .kind       = kind,
        .breakMult  = breakMult,
        .isBreaking = false,
    };

    if (((kind) == (bkVerb))) {
        result.isBreaking = true;
    };
    return result;
};

LytBlock initEmptyBlock() {
    LytBlock result;
    LytBlock{
        .id         = getBId(),
        .kind       = bkEmpty,
        .breakMult  = 1,
        .isBreaking = false,
    };
    return result;
};

seq[LytBlock] filterEmpty(const openArray<LytBlock>& blocks) {
    seq[LytBlock] result;
    for (const auto bl : blocks) {
        if (((bl.kind) != (bkEmpty))) {
            result.add(bl);
        };
    };
    return result;
};

LytBlock initTextBlock(
    const LytStrSpan& text,
    const int&        breakMult = 1,
    const bool&       breaking  = false) {
    LytBlock result;
    assert((!(breaking)));
    result = LytBlock{
        .kind       = bkText,
        .text       = text,
        .isBreaking = breaking,
        .id         = getBId(),
        .breakMult  = breakMult,
    };

    return result;
};

LytBlock initTextBlock(
    const LytStr& text,
    const int&    breakMult = 1,
    const bool&   breaking  = false) {
    LytBlock result;
    initTextBlock(lytStrSpan(text), breakMult, breaking);
    return result;
};

LytBlock initIndentBlock(
    const LytBlock& blc,
    const int&      indent,
    const int&      breakMult = 1) {
    LytBlock result;
    return result;
};

bool isEmpty(const LytBlock& bl) {
    bool result;
    ((((bl.kind) == (bkEmpty)))
     || ((
         ((((bl.kind)in({bkStack, bkLine, bkChoice})))
          && (((bl.len) == (0)))))));
    return result;
};
/* FIXME TEMPLATE */ /*

template findSingle*(elems: typed; targetKind: typed): untyped =
  var
    countEmpty = 0
    countFull = 0
    idx = -1
  for item in elems:
    if item.isEmpty():
      inc countEmpty
    elif
      when targetKind is set:
        item.kind in targetKind
       else:
        item.kind == targetKind:
      if idx != -1:
        idx = -1
        break
      else:
        idx = countFull
    inc countFull
  if countFull == countEmpty + 1 and idx != -1:
    idx
  else:
    -1

*/


int max(const std::vector<int>& ints, const int& onEmpty) {
    int result;
    if (((ints.len) == (0))) {
        onEmpty;
    } else {
        max(ints);
    };
    return result;
};

int min(const std::vector<int>& ints, const int& onEmpty) {
    int result;
    if (((ints.len) == (0))) {
        onEmpty;
    } else {
        min(ints);
    };
    return result;
};

void updateSizes(LytBlock& bk) {
    if (((((bk.kind)in({bkChoice, bkLine, bkStack})))
         && (((bk.elements.len) > (0))))) {
        bk.isBreaking = bk.elements[backIdx(1)].isBreaking;
    };
};

LytBlock convertBlock(const LytBlock& bk, const LytBlockKind& newKind) {
    LytBlock result;
    result = LytBlock{
        .id         = getBId(),
        .breakMult  = bk.breakMult,
        .kind       = newKind,
        .isBreaking = false,
    };

    result.elements = bk.elements;

    updateSizes(result);
    return result;
};

LytBlock flatten(const LytBlock& bl, const set<LytBlockKind>& kind) {
    LytBlock result;
    if (((((bl.kind)in(kind)))
         && (/* FIXME STMT LIST EXPR */

             const auto idx = findSingle(
                 bl.elements,
                 (({rangeIncl(low(LytBlockKind), high(LytBlockKind))})
                  - ({bkEmpty})));

             ((idx) != (-1))))) {
        result = bl.elements[idx];

    } else {
        result = bl;
    };
    return result;
};

LytBlock initChoiceBlock(
    const openArray<LytBlock>& elems,
    const int&                 breakMult = 1) {
    LytBlock result;
    result = LytBlock{
        .id         = getBId(),
        .isBreaking = false,
        .breakMult  = breakMult,
        .kind       = bkChoice,
        .elements   = filterEmpty(elems),
    };

    updateSizes(result);
    return result;
};

LytBlock initLineBlock(
    const openArray<LytBlock>& elems,
    const int&                 breakMult = 1) {
    LytBlock result;
    result = LytBlock{
        .id         = getBId(),
        .isBreaking = false,
        .breakMult  = breakMult,
        .kind       = bkLine,
        .elements   = filterEmpty(elems),
    };

    updateSizes(result);
    return result;
};

LytBlock initIndentBlock(
    const LytBlock& blc,
    const int&      indent,
    const int&      breakMult = 1) {
    LytBlock result;
    if (((indent) == (0))) {
        blc;
    } else {
        initLineBlock((@({initTextBlock(lytStrSpaces(indent)), blc})));
    };
    return result;
};

LytBlock initStackBlock(
    const openArray<LytBlock>& elems,
    const int&                 breakMult = 1) {
    LytBlock result;
    result = LytBlock{
        .id         = getBId(),
        .isBreaking = false,
        .breakMult  = breakMult,
        .kind       = bkStack,
        .elements   = filterEmpty(elems),
    };

    updateSizes(result);
    return result;
};

LytBlock initWrapBlock(
    const openArray<LytBlock>& elems,
    const LytStr&              sep,
    const int&                 breakMult = 1) {
    LytBlock result;
    LytBlock{
        .isBreaking   = false,
        .id           = getBId(),
        .sep          = sep,
        .kind         = bkWrap,
        .wrapElements = toSeq(elems),
        .breakMult    = breakMult,
    };
    return result;
};

LytBlock initVerbBlock(
    const openArray<LytStrSpan>& textLines,
    const bool&                  breaking  = true,
    const bool&                  firstNl   = false,
    const int&                   breakMult = 1) {
    LytBlock result;
    assert(breaking);
    result = LytBlock {
        .breakMult = breakMult, .id = getBId(), .kind = bkVerb,
        .textLines = (@(textLines)), .isBreaking = breaking,
        .firstNl = firstNl,
    };

    updateSizes(result);
    return result;
};

void add(LytBlock& target, const varargs<LytBlock>& other) {
    for (const auto bl : other) {
        if (((bl.kind) != (bkEmpty))) {
            target.elements.add(bl);
        };
    };
    updateSizes(target);
};

LytBlock initSeparated(
    const std::vector<LytBlock>& blocks,
    const bool&                  vertical,
    const LytBlock&              sep) {
    LytBlock result;
    result = else { vertical }
    else {
        initLineBlock((@({})));
    };

    if (vertical) {
        for (const auto [idx, item] : blocks) {
            if (((idx) < (((len(blocks)) - (1))))) {
                result.add(initLineBlock({item, sep}));
            } else {
                result.add(item);
            };
        };
    } else {
        for (const auto [idx, item] : blocks) {
            if (((idx) > (0))) {
                result.add(sep);
            };
            result.add(item);
        };
    };
    return result;
};

LytBlock initVSeparated(
    const std::vector<LytBlock>& blocks,
    const LytBlock&              sep) {
    LytBlock result;
    initSeparated(blocks, true, sep);
    return result;
};

LytBlock initHSeparated(
    const std::vector<LytBlock>& blocks,
    const LytBlock&              sep) {
    LytBlock result;
    initSeparated(blocks, false, sep);
    return result;
};

Option[LytSolution] doOptLayout(
    LytBlock&                   self,
    std::optional<LytSolution>& rest,
    const LytOptions&           opts) {
    Option[LytSolution] result;
    return result;
};

Option[LytSolution] optLayout(
    LytBlock&                   self,
    std::optional<LytSolution>& rest,
    const LytOptions&           opts) {
    Option[LytSolution] result;
    /*!Retrieve or compute the least-cost (optimum) layout for this
    block.
    - @arg{rest} :: text to the right of this block.
    - @ret{} :: Optimal layout for this block and the rest of the line.
    */

    if (((rest)notin(self.layoutCache))) {
        self.layoutCache[rest] = self.doOptLayout(rest, opts);
    };
    return self.layoutCache[rest];

    return result;
};

Option[LytSolution] doOptTextLayout(
    const LytBlock&             self,
    std::optional<LytSolution>& rest,
    const LytOptions&           opts) {
    Option[LytSolution] result;

    const auto span = self.text.len;

    const auto layout = initLayout((@({lytString(self.text)})));


    if (((span) >= (opts.rightMargin))) {
        result = some(initSolution(
            (@({0})),
            (@({span})),
            (@({float(
                (((((((span) - (opts.leftMargin))))
                   * (opts.leftMarginCost)))
                 + ((((((span) - (opts.rightMargin))))
                     * (opts.rightMargin)))))})),
            (@({float(((opts.leftMarginCost) + (opts.rightMarginCost)))})),
            (@({layout}))));

    } else if (((span) >= (opts.leftMargin))) {
        result = some(initSolution(
            (@({0, ((opts.rightMargin) - (span))})),
            (@({span, span})),
            (@({float(
                    (((((span) - (opts.leftMargin))))
                     * (opts.leftMarginCost))),
                float(
                    (((((opts.rightMargin) - (opts.leftMargin))))
                     * (opts.leftMarginCost)))})),
            (@({float(opts.leftMarginCost),
                float(((opts.leftMarginCost) + (opts.rightMarginCost)))})),
            (@({layout, layout}))));

    } else {
        result = some(initSolution(
            (@({0,
                ((opts.leftMargin) - (span)),
                ((opts.rightMargin) - (span))})),
            (@({span, span, span})),
            (@({float(0),
                float(0),
                float(
                    (((((opts.rightMargin) - (opts.leftMargin))))
                     * (opts.leftMarginCost)))})),
            (@({float(0),
                float(opts.leftMarginCost),
                float(((opts.leftMarginCost) + (opts.rightMarginCost)))})),
            (@({layout, layout, layout}))));
    };
    return result.withRestOfLine(rest, opts);

    return result;
};

Option[LytSolution] doOptLineLayout(
    LytBlock&                   self,
    std::optional<LytSolution>& rest,
    const LytOptions&           opts) {
    Option[LytSolution] result;
    assert(((self) != (/* NIL LIT */)));
    if (((self.elements.len) == (0))) {
        return rest;
    };

    std::vector<Vec<LytBlock>> elementLines = (@({}));


    elementLines.add((@({})));
    for (const auto [i, elt] : self.elements) {
        elementLines[backIdx(1)].add(elt);
        if (((((i) < (self.elements.high()))) && (elt.isBreaking))) {
            elementLines.add((@({})));
        };
    };
    if (((len(elementLines)) > (1))) {
        assert(((opts.formatPolicy.breakElementLines) != (/* NIL LIT */)));
        elementLines = opts.formatPolicy.breakElementLines(elementLines);
    };

    std::vector<LytSolution> lineSolns;


    for (const auto [i, ln] : mpairs(elementLines)) {

        auto lnLayout = else { ((i) == (elementLines.high)) }
        else {
            none(LytSolution);
        };


        for (const auto [idx, elt] : rmpairs(ln)) {
            lnLayout = elt.optLayout(lnLayout, opts);
        };
        if (lnLayout.isSome()) {
            lineSolns.add(lnLayout.get());
        };
    };

    const auto soln = vSumSolution(lineSolns);


    result = some(soln.plusConst(
        float(((opts.linebreakCost) * ((((len(lineSolns)) - (1))))))));

    return result;
};

Option[LytSolution] doOptChoiceLayout(
    LytBlock&                   self,
    std::optional<LytSolution>& rest,
    const LytOptions&           opts) {
    Option[LytSolution] result;
    return minSolution(

        std::vector<LytSolution> tmp;;; for (const auto it
                                             : mitems(self.elements)) {
            const auto lyt = it.optLayout(rest, opts);


            if (lyt.isSome()) {
                tmp.add(lyt.get());
            };
        };
        tmp;);

    return result;
};

Option[LytSolution] doOptStackLayout(
    LytBlock&                   self,
    std::optional<LytSolution>& rest,
    const LytOptions&           opts) {
    Option[LytSolution] result;
    if (((self.elements.len) == (0))) {
        return rest;
    };

    const auto soln = vSumSolution(
        get(collect(
                newSeq,
                for (const auto [idx, elem]
                     : mpairs(self.elements)) {
                    if (((idx) < (self.elements.high))) {

                        auto it = none(LytSolution);


                        optLayout(elem, it, opts);
                    } else {
                        elem.optLayout(rest, opts);
                    };
                };);););


    if (((soln.layouts.len) == (0))) {
        return rest;
    };
    return some(soln.plusConst(float(
        ((((opts.linebreakCost) * (self.breakMult)))
         * (max(((len(self.elements)) - (1)), 0))))));

    return result;
};

Option[LytSolution] doOptWrapLayout(
    LytBlock&                   self,
    std::optional<LytSolution>& rest,
    const LytOptions&           opts) {
    Option[LytSolution] result;
    /*!Computing the optimum layout for this class of block involves
    finding the optimal packing of elements into lines, a problem
    which we address using dynamic programming.
    */


    auto sepLayout = //
    {

        auto it = {
            initTextBlock(self.sep),
            none(LytSolution),
        };


    it[0].optLayout(it[1], opts);
};


std::optional<LytSolution> prefixLayout = else { self.prefix.isSome() }
else {
    none(LytSolution);
};


auto eltLayouts = //
{

    std::vector<std::optional<LytSolution>> res;


for (const auto it : mitems(self.wrapElements)) {

    auto tmp = none(LytSolution);


    res.add(it.optLayout(tmp, opts));
};
res;
}


std::vector<std::optional<LytSolution>>
    wrapSolutions = self.len.newSeqWith(none(LytSolution));


for (const auto i : countdown(((self.len) - (1)), 0)) {

    std::vector<LytSolution> solutionsI;


    std::optional<LytSolution> lineLayout = else { prefixLayout.isNone() }
    else {
        prefixLayout.withRestOfLine(eltLayouts[i], opts);
    };


    auto breakOut = false;


    bool lastBreaking = self.wrapElements[i].isBreaking;


    for (int j = i; j < ((self.len) - (1)); ++i) {

        const auto fullSoln = vSumSolution(
            (@({lineLayout.withRestOfLine(sepLayout, opts),
                wrapSolutions[((j) + (1))]}))
                .get());


        solutionsI.add(plusConst(
            fullSoln,
            float(
                ((((opts.linebreakCost) * (self.breakMult)))
                 + (((opts.cpack) * ((((self.len) - (j))))))))));
        if (lastBreaking) {
            breakOut = true;

            break;
        };

        auto sepEltLayout = sepLayout.withRestOfLine(
            eltLayouts[((j) + (1))], opts);


        lineLayout = lineLayout.withRestOfLine(sepEltLayout, opts);

        lastBreaking = self.wrapElements[((j) + (1))].isBreaking;
    };
    if ((!(breakOut))) {

        auto line = lineLayout.withRestOfLine(rest, opts);


        solutionsI.add(line.get());
    };
    wrapSolutions[i] = minSolution(solutionsI);
};
return wrapSolutions[0];

return result;
}


Option[LytSolution] doOptVerbLayout(
    LytBlock&                   self,
    std::optional<LytSolution>& rest,
    const LytOptions&           opts) {
    Option[LytSolution] result;

    std::vector<LayoutElement> lElts;


    for (const auto [i, ln] : self.textLines) {
        if (((((i) > (0))) || (self.firstNl))) {
            lElts.add(lytNewline());
        };
        lElts.add(lytString(ln));
    };

    const auto layout = initLayout(lElts);


    const auto span = 0;


    LytSolution sf;


    new (sf);
    if (((opts.leftMargin) > (0))) {
        sf.add(0, span, 0, 0, layout);
    };
    sf.add(
        ((opts.leftMargin) - (span)),
        span,
        0,
        opts.leftMarginCost,
        layout);
    sf.add(
        ((opts.rightMargin) - (span)),
        span,
        (((((opts.rightMargin) - (opts.leftMargin))))
         * (opts.leftMarginCost)),
        ((opts.leftMarginCost) + (opts.rightMarginCost)),
        layout);
    result = some(sf);

    return result;
};

Option[LytSolution] doOptLayout(
    LytBlock&                   self,
    std::optional<LytSolution>& rest,
    const LytOptions&           opts) {
    Option[LytSolution] result;
    switch (self.kind) {
        case bkText: {
            result = self.doOptTextLayout(rest, opts);

            break;
        }
        case bkLine: {
            result = self.doOptLineLayout(rest, opts);

            break;
        }
        case bkChoice: {
            result = self.doOptChoiceLayout(rest, opts);

            break;
        }
        case bkStack: {
            result = self.doOptStackLayout(rest, opts);

            break;
        }
        case bkWrap: {
            result = self.doOptWrapLayout(rest, opts);

            break;
        }
        case bkVerb: {
            result = self.doOptVerbLayout(rest, opts);

            break;
        }
        case bkEmpty: {
            assert(false);
            break;
        }
    };
    return result;
};

LytOptions initLytOptions() {
    LytOptions result;
    result = LytOptions{.leftMargin = 0,.rightMargin = 80,.leftMarginCost = 0.05,.rightMarginCost = 100,.linebreakCost = 5,.indentSpaces = 2,.cpack = 0.001,.formatPolicy = LytFormatPolicy{.breakElementLines = ([]( const std::vector<Vec<LytBlock>>& blc   ){
LytBlock strippedLine( const std::vector<LytBlock>& line   ) {LytBlock result;
return initLineBlock(line);; return result;};
result.add((@({blc[0]})));
  if (((blc.len) > (1))) {

const auto ind  = initIndentBlock(initStackBlock(blc[((1) ..^ (1))].map(strippedLine)), ((2) * (2))) ; ;;
result.add((@({ind})));  };}),},};

    return result;
};

LytBlock join(
    const LytBlock& blocks,
    const LytBlock& sep,
    const bool&     vertLines = true) {
    LytBlock result;
    assert(
        ((blocks.kind)in({bkLine, bkStack})),
        R"(Only stack or line layouts can be joined)");
    result = initBlock(blocks.kind);

    for (const auto [idx, item] : pairs(blocks)) {

        const auto isLast = ((idx) == (((len(blocks)) - (1))));


        if (((((blocks.kind) == (bkStack))) && (vertLines))) {
            if ((!(isLast))) {
                result.add(initLineBlock({item, sep}));
            } else {
                result.add(item);
            };
        } else {
            result.add(item);
            if ((!(isLast))) {
                result.add(sep);
            };
        };
    };
    return result;
};

LytBlock join(
    const std::vector<LytBlock>& blocks,
    const LytBlock&              sep,
    const LytBlockKind&          direction) {
    LytBlock result;
    result = initBlock(direction);

    for (const auto [idx, item] : pairs(blocks)) {

        const auto isLast = ((idx) == (((len(blocks)) - (1))));


        result.add(item);
        if ((!(isLast))) {
            result.add(sep);
        };
    };
    return result;
};


void expectValid(const LytBlock& bl, const Str& msg = R"()") {

    const auto ok = (!(
        ((((((((bl.kind)in({bkStack, bkLine, bkChoice})))
             && (((bl.elements.len) == (0))))))
          || ((
              ((((bl.kind)in({bkWrap})))
               && (((bl.wrapElements.len) == (0))))))))));


    assert(
        ok,
        ((((((((R"(Invalid combinator layout block passed - no nested elements )")
               & (R"(specified, so layout is impossible. Block kind - )")))
             & (toStr(bl.kind))))
           & (R"(. )")))
         & (msg)));
};

seq[Layout] toLayouts(const LytBlock& bl, const LytOptions& opts) {
    seq[Layout] result;
    /*!Return all possible formatting layouts for a given block with
    provided options. The best layout will be the first in the returned
    sequence.
    */

    expectValid(bl);
    switch (bl.kind) {
        case bkStack:
        case bkChoice:
        case bkLine: {
            assert(((0) < (bl.elements.len)));
            break;
        }
        case bkWrap: {
            assert(((0) < (bl.wrapElements.len)));
            break;
        }
        default: {
        }
    };

    auto bl = bl;


    const auto sln = //
    {

        auto it = none(LytSolution);


    bl.doOptLayout(it, opts);
};


assert(
    isSome(sln),
    ((R"(Could not perform layout for block )") & (toStr(bl))));
return sln.get().layouts;

return result;
}


Layout toLayout(const LytBlock& bl, const LytOptions& opts) {
    Layout result;
    /*!Return first best formatting layout for a given block. This is
    the procedure you should be using unless you need to have access to
    all the possible layouts.
    */

    toLayouts(bl, opts)[0];
    return result;
};

std::vector<LytEvent> formatEvents(const Layout& lyt) {
    std::vector<LytEvent> result{};
    /*!Generate formatting events for the given layout. The events are
    backend-agnostic and can be interpreted further by the user
    depending on their needs.
    */


    OutConsole buf;


    std::vector<std::tuple<Layout lyt, int idx>> stack = (@({{
        lyt,
        0,
    }}));


    /* FIXME TEMPLATE */ /*

    template top(): untyped =
      stack[^1]

    */

    buf.addMargin(buf.hPos);
    while (((0) < (len(stack)))) {
        while (((top().idx) < (top().lyt.elements.len))) {

            const auto elem = top().lyt.elements[top().idx];


            += top().idx;
            buf.hPos = max(buf.hPos, buf.margin());

            switch (elem.kind) {
                case lekString: {
                    for (const auto item : elem.text.strs) {
                        if (item.id.isNil()) {

                        } else if (((item.id) == (LytSpacesId))) {
                            if (((item.len) != (0))) {
                                result.push_back(LytEvent{
                                    .kind   = layEvSpaces,
                                    .spaces = item.len,
                                });
                            };
                            ((buf.hPos) += (item.len));
                        } else {
                            result.push_back(LytEvent{
                                .kind = layEvStr,
                                .str  = item,
                            });
                            ((buf.hPos) += (item.len));
                        };
                    };
                    break;
                }
                case lekNewline:
                case lekNewlineSpace: {
                    result.push_back(LytEvent{
                        .kind = layEvNewline,
                    });
                    buf.hPos = 0;


                    const auto mar = buf.margin();


                    if (((mar) != (0))) {
                        result.push_back(LytEvent{
                            .kind   = layEvSpaces,
                            .spaces = mar,
                        });
                        ((buf.hPos) += (mar));
                    };
                    if (((((elem.kind) == (lekNewlineSpace)))
                         && (((elem.spaceNum) != (0))))) {
                        result.push_back(LytEvent{
                            .kind   = layEvSpaces,
                            .spaces = elem.spaceNum,
                        });
                        ((buf.hPos) += (elem.spaceNum));
                    };
                    break;
                }
                case lekLayoutPrint: {
                    stack.add({
                        elem.layout,
                        0,
                    });
                    buf.addMargin(buf.hPos);
                    break;
                }
            };
        };
        stack.pop();
        buf.popMargin();
    };
    return result;
};

LytStr lytSpaces(const int& count = 1) {
    LytStr result;
    result.id = LytSpacesId;

    result.len = count;

    return result;
};

void add(LytStrSpan& span, const LytStr& str) {
    span.strs.add(str);
    ((span.len) += (str.len));
};

LytStrSpan alignLeft(const LytStrSpan& span, const int& target) {
    LytStrSpan result;
    result = span;

    if (((result.len) < (target))) {
        result.add(lytSpaces(((target) - (result.len))));
    };
    return result;
};


enum LytAlignDirection
{
    lAlignLeft,
    lAlignRight,
    lAlignCenter,
};
int textLen(const LytBlock& b) {
    int result;
    switch (b.kind) {
        case bkText: {
            result = b.text.len;

            break;
        }
        case bkChoice: {
            for (const auto item : b.elements) {

                const auto len = textLen(item);


                result = min(result, len);
            };
            break;
        }
        case bkStack: {
            for (const auto item : b.elements) {

                const auto len = textLen(item);


                result = max(result, len);
            };
            break;
        }
        case bkLine: {
            for (const auto item : b.elements) {
                ((result) += (textLen(item)));
            };
            break;
        }
        default: {
        }
    };
    return result;
};

LytBlock initAlignedGrid(
    const std::vector<Vec<LytBlock>>&  blocks,
    const openArray<std::tuple<
        int leftPad int rightPad,
        ,
        LytAlignDirection direction>>& aligns) {
    LytBlock result;
    for (const auto [idx, row] : pairs(blocks)) {
        assert(
            ((len(row)) <= (len(aligns))),
            ((((((((((((R"(Invalid number for column alignments specified - row )")
                       & (toStr(idx))))
                     & (R"( has total of )")))
                   & (toStr(len(row)))))
                 & (R"( cells, but only )")))
               & (toStr(len(aligns)))))
             & (R"( were specified.)")));
    };

    auto colWidths = newSeqWith(len(aligns), 0);


    for (const auto [rowIdx, row] : pairs(blocks)) {
        for (const auto [colIdx, col] : pairs(row)) {
            colWidths[colIdx] = max(textLen(col), colWidths[colIdx]);
        };
    };
    result = initStackBlock({});

    for (const auto row : items(blocks)) {

        auto resRow = initLineBlock({});


        for (const auto [idx, col] : pairs(row)) {

            const auto al = aligns[idx];


            const auto diff = ((colWidths[idx]) - (textLen(col)));


            switch (al.direction) {
                case lAlignLeft: {
                    resRow.add(initLineBlock(
                        {lytSpaces(al.leftPad).initTextBlock(),
                         col,
                         lytSpaces(((al.rightPad) + (diff)))
                             .initTextBlock()}));
                    break;
                }
                case lAlignRight: {
                    resRow.add(initLineBlock(
                        {lytSpaces(((al.leftPad) + (diff)))
                             .initTextBlock(),
                         col,
                         lytSpaces(al.rightPad).initTextBlock()}));
                    break;
                }
                case lAlignCenter: {

                    const auto left = ((diff)div(2));


                    const auto right = ((diff) - (left));


                    resRow.add(initLineBlock(
                        {lytSpaces(((al.leftPad) + (left)))
                             .initTextBlock(),
                         col,
                         lytSpaces(((al.rightPad) + (right)))
                             .initTextBlock()}));
                    break;
                }
            };
        };
        result.add(resRow);
    };
    return result;
};

LytBlock initAlignedGrid(
    const std::vector<Vec<LytBlock>>&   blocks,
    const openArray<LytAlignDirection>& aligns) {
    LytBlock result;
    initAlignedGrid(
        blocks,
        mapIt(
            aligns,
            {
                0,
                0,
                it,
            }));
    return result;
};
