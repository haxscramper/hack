import std/[sequtils, tables, strformat]

type
  Terminal = object
    str: string

  RuleKind = enum
    ruleTerminal
    ruleChoice
    ruleSequence
    ruleNot
    ruleRepetition
    ruleCustom
    ruleRule

  Rule = ref object
    case kind*: RuleKind
      of ruleChoice, ruleSequence:
        exps: seq[Rule]

      of ruleTerminal:
        str: string

      of ruleNot, ruleRepetition:
        exp: Rule

      of ruleRule:
        ruleName: string

      of ruleCustom:
        cb: proc(matcher: var Matcher): Cst

  MemoCol = ref object
    memo: Table[string, MemoEntry]
    maxExaminedLength: int


  MemoEntry = object
    cst: Cst
    matchLength: int
    examinedLength: int

  Cst = ref object
    str: string
    sub: seq[Cst]

  Matcher = object
    pos: int
    maxExaminedPos: int
    rules: Table[string, Rule]
    memoTable: seq[MemoCol]
    input: string

proc newCustom(rule: proc(matcher: var Matcher): Cst): auto =
  Rule(kind: ruleCustom, cb: rule)

proc newTerminal(str: string): auto = Rule(kind: ruleTerminal, str: str)
proc newChoice(exps: varargs[Rule]): auto = Rule(kind: ruleChoice, exps: toSeq(exps))
proc newSequence(exps: varargs[Rule]): auto =
  Rule(kind: ruleSequence, exps: toSeq(exps))
proc newMatcher(rules: openarray[(string, Rule)]): auto =
  Matcher(rules: toTable(rules))

proc newNot(exp: Rule): auto = Rule(kind: ruleNot, exp: exp)
proc newRep(exp: Rule): auto = Rule(kind: ruleRepetition, exp: exp)

proc newRuleApplication(name: string): auto =
  Rule(kind: ruleRule, ruleName: name)


proc hasMemoizedResult(this: var Matcher, ruleName: string): bool =
  this.memoTable[this.pos].isNil().not() and
  ruleName in this.memoTable[this.pos].memo

proc useMemoizedResult(this: var Matcher, ruleName: string): Cst =
  var col = this.memoTable[this.pos]
  var res = col.memo[ruleName]
  this.maxExaminedPos = max(
      this.maxExaminedPos,
      this.pos + res.examinedLength - 1)

  if not isNil(res.cst):
    this.pos += res.matchLength
    return res.cst

  return nil



proc memoizeResult(this: var Matcher, pos: int, ruleName: string, cst: Cst) =
  if isNil(this.memoTable[pos]):
    this.memoTable[pos] = MemoCol(maxExaminedLength: -1)

  var col: MemoCol = this.memoTable[pos]



  var examinedLength =
      this.maxExaminedPos - pos + 1;

  if isNil(cst):
    col.memo[ruleName] = MemoEntry(
      cst: cst,
      matchLength: this.pos - pos,
      examinedLength: examinedLength
    )

  else:
    col.memo[ruleName] = MemoEntry(
      examinedLength: examinedLength)

  col.maxExaminedLength = max(
      col.maxExaminedLength,
      examinedLength)



proc consume(this: var Matcher, c: char): bool =
  this.maxExaminedPos = max(this.maxExaminedPos, this.pos);
  if (this.input[this.pos] == c):
    inc this.pos
    return true

  return false


proc eval(this: Rule, matcher: var Matcher): Cst =
  case this.kind:
    of ruleTerminal:
      for ch in this.str:
        if not matcher.consume(ch):
          return nil

      return Cst(str: this.str)

    of ruleChoice:
      var origPos = matcher.pos
      for exp in this.exps:
        matcher.pos = origPos
        var cst = exp.eval(matcher)
        if isNil(cst):
          return cst

      return nil

    of ruleSequence:
      var ans = Cst()
      for exp in this.exps:
        var cst = exp.eval(matcher)
        if isNil(cst):
          return nil

        if exp.kind != ruleNot:
          ans.sub.add cst

      return ans

    of ruleNot:
      var origPos = matcher.pos
      let eval = this.exp.eval(matcher)
      if isNil(eval):
        matcher.pos = origPos
        return Cst()

      return nil

    of ruleRepetition:
      var ans = Cst()
      while (true):
        var origPos = matcher.pos
        var cst = this.exp.eval(matcher)
        if isNil(cst):
          matcher.pos = origPos;
          break

        else:
          ans.sub.add cst

      return ans

    of ruleCustom:
      return this.cb(matcher)


    of ruleRule:
      var name = this.ruleName;
      if (matcher.hasMemoizedResult(name)):
        return matcher.useMemoizedResult(name)

      else:
        var
          origPos = matcher.pos
          origMax = matcher.maxExaminedPos

        matcher.maxExaminedPos = -1
        var cst = matcher.rules[name].eval(matcher)
        matcher.memoizeResult(origPos, name, cst)
        matcher.maxExaminedPos = max(
          matcher.maxExaminedPos, origMax)
        return cst


proc match(this: var Matcher): Cst =
  this.pos = 0;
  this.maxExaminedPos = -1;
  var cst = newRuleApplication("start").eval(this)

  if (this.pos == this.input.len):
    return cst

  else:
    return nil



import hmisc/other/hpprint

proc applyEdit(this: var Matcher, startPos, endPos: int, r: string) =
  var s = this.input;
  var m = this.memoTable;

  assert startPos <= s.high or (s.len() == 0 and startPos == 0),
    &"start: {startPos}, high: {s.high}"

  if startPos != 0:
    this.input = s[0 .. startPos]
    this.memoTable = m[0 .. startPos]

  this.input.add r
  this.memoTable.add newSeqWith(r.len, MemoCol(nil))

  assert endPos == r.high or endPos <= s.high

  if endPos != r.high:
    this.input.add s[endPos .. ^1]
    this.memoTable.add m[endPos .. ^1]

  for pos in 0 ..< startPos:
    var col = m[pos]
    if not isNil(col) and pos + col.maxExaminedLength > startPos:
      var newMax = 0
      for ruleName, entry in col.memo:
        var examinedLen = entry.examinedLength
        if (pos + examinedLen > startPos):
          col.memo.del(ruleName)

        elif (examinedLen > newMax):
          newMax = examinedLen;

      col.maxExaminedLength = newMax;


when isMainModule:
  proc tok(value: string): auto = newTerminal(value)
  proc app(name: string): auto = newRuleApplication(name)
  proc seq(exps: varargs[Rule]): auto = newSequence(exps)
  proc choice(exps: varargs[Rule]): auto = newChoice(exps)
  proc rep(exp: Rule): auto = newRep(exp)
  proc rnot(exp: Rule): auto = newNot(exp)
  proc lookahead(exp: Rule): auto = newNot(newNot(exp))

  proc rrange(start, finish: string): auto = newCustom(
    proc(matcher: var Matcher): Cst =
      assert start.len == finish.len
      let startPos = matcher.pos
      var i = 0
      while
        i < start.len and
        start[i] < matcher.input[matcher.pos] and
        matcher.input[matcher.pos] <= finish[i]:
          inc matcher.pos
          inc i


      if i == start.len:
        return Cst(str: matcher.input[startPos ..< matcher.pos])

      else:
        return nil
  )

  var matcher = newMatcher {
    "start": app("expr"),
    "expr": choice(
      seq(app("num"), tok("+"), app("num")),
      seq(app("num"), tok("-"), app("num"))
    ),
    "num": rep(app("digit")),
    "digit": rrange("0", "9")
  }

  let str = "896-7"

  matcher.applyEdit(0, str.high, str)
  let res = matcher.match()

  pprint matcher.memoTable
