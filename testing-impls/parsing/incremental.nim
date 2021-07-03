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
    memo: TableRef[string, MemoEntry]
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
  # this.memoTable[this.pos].memo.isNil().not() and
  ruleName in this.memoTable[this.pos].memo

proc useMemoizedResult(this: var Matcher, ruleName: string): Cst =
  var col = this.memoTable[this.pos]
  var result = col.memo[ruleName]
  this.maxExaminedPos = max(
      this.maxExaminedPos,
      this.pos + result.examinedLength - 1)

  if not isNil(result.cst):
    this.pos += result.matchLength
    return result.cst

  return nil



proc memoizeResult(this: var Matcher, pos: int, ruleName: string, cst: Cst) =
  var col: MemoCol = this.memoTable[pos]
  if isNil(col):
    col = MemoCol(
      memo: newTable[string, MemoEntry](), maxExaminedLength: -1)


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

  # Step 1: Apply edit to the input
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

  # this.input = s[0 .. startPos] & r & s[endPos ..^ 1]

  # Step 2: Adjust memo table


    # m.slice(0, startPos).concat(
    #   new Array(r.length).fill(null),
    #   m.slice(endPos));

  # Step 3: Invalidate overlapping entries
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

  # const lexIgnored = exp => exp;

  var matcher = newMatcher {
    "start": app("program"),
    "any": rrange("\u0000", "\uFFFF"),
    "end": rnot(app("any")),
    "program": seq(lookahead(rep(app("directive"))), rep(app("sourceElement")), app("spaces")),
    "sourceCharacter": app("any"),
    "space": choice(app("whitespace"), app("lineTerminator"), app("comment")),
    "spaces": rep(app("space")),
    "whitespace": choice(tok("\t"), tok("\x0B"), tok("\x0C"), tok(" "), tok("\u00A0"), tok("\uFEFF"), app("unicodeSpaceSeparator")),
    "lineTerminator": choice(tok("\n"), tok("\r"), tok("\u2028"), tok("\u2029")),
    "lineTerminatorSequence": choice(tok("\n"), seq(tok("\r"), rnot(tok("\n"))), tok("\u2028"), tok("\u2029"), tok("\r\n")),
    "comment": choice(app("multiLineComment"), app("singleLineComment")),
    "multiLineComment": seq(tok("/*"), rep(seq(rnot(tok("*/")), app("sourceCharacter"))), tok("*/")),
    "singleLineComment": seq(tok("//"), rep(seq(rnot(app("lineTerminator")), app("sourceCharacter")))),
    "identifier": seq(rnot(app("reservedWord")), app("identifierName")),
    "identifierName": seq(app("identifierStart"), rep(app("identifierPart"))),
    "identifierStart": choice(app("letter"), tok("$"), tok("_"), seq(tok("\\"), app("unicodeEscapeSequence"))),
    "identifierPart": choice(app("identifierStart"), app("unicodeCombiningMark"), app("unicodeDigit"), app("unicodeConnectorPunctuation"), tok("\u200C"), tok("\u200D")),
    "letter": choice(rrange("a", "z"), rrange("A", "Z"), rrange("À", "ÿ"), app("unicodeCategoryNl")),
    "unicodeCategoryNl": choice(rrange("\u2160", "\u2182"), tok("\u3007"), rrange("\u3021", "\u3029")),
    "unicodeDigit": choice(rrange("\u0030", "\u0039"), rrange("\u0660", "\u0669"), rrange("\u06F0", "\u06F9"), rrange("\u0966", "\u096F"), rrange("\u09E6", "\u09EF"),
     rrange("\u0A66", "\u0A6F"), rrange("\u0AE6", "\u0AEF"), rrange("\u0B66", "\u0B6F"), rrange("\u0BE7", "\u0BEF"), rrange("\u0C66", "\u0C6F"), rrange("\u0CE6", "\u0CEF"),
     rrange("\u0D66", "\u0D6F"), rrange("\u0E50", "\u0E59"), rrange("\u0ED0", "\u0ED9"), rrange("\u0F20", "\u0F29"), rrange("\uFF10", "\uFF19")),
    "unicodeCombiningMark": choice(
      rrange("\u0300", "\u0345"),
      rrange("\u0360", "\u0361"),
      rrange("\u0483", "\u0486"),
      rrange("\u0591", "\u05A1"),
      rrange("\u05A3", "\u05B9"),
      rrange("\u05BB", "\u05BD"),
      rrange("\u05BF", "\u05BF"),
      rrange("\u05C1", "\u05C2"),
      rrange("\u05C4", "\u05C4"),
      rrange("\u064B", "\u0652"),
      rrange("\u0670", "\u0670"),
      rrange("\u06D6", "\u06DC"),
      rrange("\u06DF", "\u06E4"),
      rrange("\u06E7", "\u06E8"),
      rrange("\u06EA", "\u06ED"),
      rrange("\u0901", "\u0902"),
      rrange("\u093C", "\u093C"),
      rrange("\u0941", "\u0948"),
      rrange("\u094D", "\u094D"),
      rrange("\u0951", "\u0954"),
      rrange("\u0962", "\u0963"),
      rrange("\u0981", "\u0981"),
      rrange("\u09BC", "\u09BC"),
      rrange("\u09C1", "\u09C4"),
      rrange("\u09CD", "\u09CD"),
      rrange("\u09E2", "\u09E3"),
      rrange("\u0A02", "\u0A02"),
      rrange("\u0A3C", "\u0A3C"),
      rrange("\u0A41", "\u0A42"),
      rrange("\u0A47", "\u0A48"),
      rrange("\u0A4B", "\u0A4D"),
      rrange("\u0A70", "\u0A71"),
      rrange("\u0A81", "\u0A82"),
      rrange("\u0ABC", "\u0ABC"),
      rrange("\u0AC1", "\u0AC5"),
      rrange("\u0AC7", "\u0AC8"),
      rrange("\u0ACD", "\u0ACD"),
      rrange("\u0B01", "\u0B01"),
      rrange("\u0B3C", "\u0B3C"),
      rrange("\u0B3F", "\u0B3F"),
      rrange("\u0B41", "\u0B43"),
      rrange("\u0B4D", "\u0B4D"),
      rrange("\u0B56", "\u0B56"),
      rrange("\u0B82", "\u0B82"),
      rrange("\u0BC0", "\u0BC0"),
      rrange("\u0BCD", "\u0BCD"),
      rrange("\u0C3E", "\u0C40"),
      rrange("\u0C46", "\u0C48"),
      rrange("\u0C4A", "\u0C4D"),
      rrange("\u0C55", "\u0C56"),
      rrange("\u0CBF", "\u0CBF"),
      rrange("\u0CC6", "\u0CC6"),
      rrange("\u0CCC", "\u0CCD"),
      rrange("\u0D41", "\u0D43"),
      rrange("\u0D4D", "\u0D4D"),
      rrange("\u0E31", "\u0E31"),
      rrange("\u0E34", "\u0E3A"),
      rrange("\u0E47", "\u0E4E"),
      rrange("\u0EB1", "\u0EB1"),
      rrange("\u0EB4", "\u0EB9"),
      rrange("\u0EBB", "\u0EBC"),
      rrange("\u0EC8", "\u0ECD"),
      rrange("\u0F18", "\u0F19"),
      rrange("\u0F35", "\u0F35"),
      rrange("\u0F37", "\u0F37"),
      rrange("\u0F39", "\u0F39"),
      rrange("\u0F71", "\u0F7E"),
      rrange("\u0F80", "\u0F84"),
      rrange("\u0F86", "\u0F87"),
      rrange("\u0F90", "\u0F95"),
      rrange("\u0F97", "\u0F97"),
      rrange("\u0F99", "\u0FAD"),
      rrange("\u0FB1", "\u0FB7"),
      rrange("\u0FB9", "\u0FB9"),
      rrange("\u20D0", "\u20DC"),
      rrange("\u20E1", "\u20E1"),
      rrange("\u302A", "\u302F"),
      rrange("\u3099", "\u309A"),
      rrange("\uFB1E", "\uFB1E"),
      rrange("\uFE20", "\uFE23")
    ),
    "unicodeConnectorPunctuation": choice(
      tok("\u005F"),
      rrange("\u203F", "\u2040"),
      tok("\u30FB"),
      rrange("\uFE33", "\uFE34"),
      rrange("\uFE4D", "\uFE4F"),
      tok("\uFF3F"),
      tok("\uFF65")
    ),
    "unicodeSpaceSeparator": choice(rrange("\u2000", "\u200B"), tok("\u3000")),
    "reservedWord": choice(app("keyword"), app("futureReservedWord"), app("nullLiteral"), app("booleanLiteral")),
    "keyword": choice(app("break"), app("do"), app("instanceof"), app("typeof"), app("case"), app("else"), app("new"), app("var"), app("catch"), app("finally"), app("return"), app("void"), app("continue"), app("for"), app("switch"), app("while"), app("debugger"), app("function"), app("this"), app("with"), app("default"), app("if"), app("throw"), app("delete"), app("in"), app("try")),
    "futureReservedWordLax": choice(app("class"), app("enum"), app("extends"), app("super"), app("const"), app("export"), app("import")),
    "futureReservedWordStrict": choice(app("futureReservedWordLax"), app("implements"), app("let"), app("private"), app("public"), app("interface"), app("package"), app("protected"),
    app("static"), app("yield")),
    "futureReservedWord": app("futureReservedWordStrict"),
    "literal": choice(app("nullLiteral"), app("booleanLiteral"), app("numericLiteral"), app("stringLiteral"), app("regularExpressionLiteral")),
    "nullLiteral": seq(tok("null"), rnot(app("identifierPart"))),
    "booleanLiteral": seq(choice(tok("true"), tok("false")), rnot(app("identifierPart"))),
    "numericLiteral": choice(app("octalIntegerLiteral"), app("hexIntegerLiteral"), app("decimalLiteral")),
    "decimalLiteral": choice(seq(app("decimalIntegerLiteral"), tok("."), rep(app("decimalDigit")), app("exponentPart")), seq(tok("."), seq(app("decimalDigit"), rep(app("decimalDigit"))),
     app("exponentPart")), seq(app("decimalIntegerLiteral"), app("exponentPart"))),
    "decimalIntegerLiteral": choice(seq(app("nonZeroDigit"), rep(app("decimalDigit"))), tok("0")),
    "decimalDigit": rrange("0", "9"),
    "nonZeroDigit": rrange("1", "9"),
    "exponentPart": choice(seq(app("exponentIndicator"), app("signedInteger")), seq()),
    "exponentIndicator": choice(tok("e"), tok("E")),
    "signedInteger": choice(seq(tok("+"), rep(app("decimalDigit"))), seq(tok("-"), rep(app("decimalDigit"))), seq(app("decimalDigit"), rep(app("decimalDigit")))),
    "hexIntegerLiteral": choice(seq(tok("0x"), seq(app("hexDigit"), rep(app("hexDigit")))), seq(tok("0X"), seq(app("hexDigit"), rep(app("hexDigit"))))),
    "hexDigit": choice(rrange("0", "9"), rrange("a", "f"), rrange("A", "F")),
    "octalIntegerLiteral": seq(tok("0"), seq(app("octalDigit"), rep(app("octalDigit")))),
    "octalDigit": rrange("0", "7"),
    "stringLiteral": choice(seq(tok("\""), rep(app("doubleStringCharacter")), tok("\"")), seq(tok("'"), rep(app("singleStringCharacter")), tok("'"))),
    "doubleStringCharacter": choice(seq(rnot(choice(tok("\""), tok("\\"), app("lineTerminator"))), app("sourceCharacter")), seq(tok("\\"), app("escapeSequence")), app("lineContinuation")),
    "singleStringCharacter": choice(seq(rnot(choice(tok("'"), tok("\\"), app("lineTerminator"))), app("sourceCharacter")), seq(tok("\\"), app("escapeSequence")), app("lineContinuation")),
    "lineContinuation": seq(tok("\\"), app("lineTerminatorSequence")),
    "escapeSequence": choice(app("unicodeEscapeSequence"), app("hexEscapeSequence"), app("octalEscapeSequence"), app("characterEscapeSequence")),
    "characterEscapeSequence": choice(app("singleEscapeCharacter"), app("nonEscapeCharacter")),
    "singleEscapeCharacter": choice(tok("'"), tok("\""), tok("\\"), tok("b"), tok("f"), tok("n"), tok("r"), tok("t"), tok("v")),
    "nonEscapeCharacter": seq(rnot(choice(app("escapeCharacter"), app("lineTerminator"))), app("sourceCharacter")),
    "escapeCharacter": choice(app("singleEscapeCharacter"), app("decimalDigit"), tok("x"), tok("u")),
    "octalEscapeSequence": choice(seq(app("zeroToThree"), app("octalDigit"), app("octalDigit")), seq(app("fourToSeven"), app("octalDigit")), seq(app("zeroToThree"), app("octalDigit"),
    rnot(app("decimalDigit"))), seq(app("octalDigit"), rnot(app("decimalDigit")))),
    "hexEscapeSequence": seq(tok("x"), app("hexDigit"), app("hexDigit")),
    "unicodeEscapeSequence": seq(tok("u"), app("hexDigit"), app("hexDigit"), app("hexDigit"), app("hexDigit")),
    "zeroToThree": rrange("0", "3"),
    "fourToSeven": rrange("4", "7"),
    "regularExpressionLiteral": seq(tok("/"), app("regularExpressionBody"), tok("/"), app("regularExpressionFlags")),
    "regularExpressionBody": seq(app("regularExpressionFirstChar"), rep(app("regularExpressionChar"))),
    "regularExpressionFirstChar": choice(seq(rnot(choice(tok("*"), tok("\\"), tok("/"), tok("["))), app("regularExpressionNonTerminator")), app("regularExpressionBackslashSequence"),
     app("regularExpressionClass")),
    "regularExpressionChar": choice(seq(rnot(choice(tok("\\"), tok("/"), tok("["))), app("regularExpressionNonTerminator")), app("regularExpressionBackslashSequence"), app("regularExpressionClass")),
    "regularExpressionBackslashSequence": seq(tok("\\"), app("regularExpressionNonTerminator")),
    "regularExpressionNonTerminator": seq(rnot(app("lineTerminator")), app("sourceCharacter")),
    "regularExpressionClass": seq(tok("["), rep(app("regularExpressionClassChar")), tok("]")),
    "regularExpressionClassChar": choice(seq(rnot(choice(tok("]"), tok("\\"))), app("regularExpressionNonTerminator")), app("regularExpressionBackslashSequence")),
    "regularExpressionFlags": rep(app("identifierPart")),
    "multiLineCommentNoNL": seq(tok("/*"), rep(seq(rnot(choice(tok("*/"), app("lineTerminator"))), app("sourceCharacter"))), tok("*/")),
    "spacesNoNL": rep(choice(app("whitespace"), app("singleLineComment"), app("multiLineCommentNoNL"))),
    "sc": choice(seq(rep(app("space")), choice(tok(";"), app("end"))), seq(app("spacesNoNL"), choice(app("lineTerminator"), seq(rnot(app("multiLineCommentNoNL")), app("multiLineComment")),
     lookahead(tok("}"))))),
    "break": seq(tok("break"), rnot(app("identifierPart"))),
    "do": seq(tok("do"), rnot(app("identifierPart"))),
    "instanceof": seq(tok("instanceof"), rnot(app("identifierPart"))),
    "typeof": seq(tok("typeof"), rnot(app("identifierPart"))),
    "case": seq(tok("case"), rnot(app("identifierPart"))),
    "else": seq(tok("else"), rnot(app("identifierPart"))),
    "new": seq(tok("new"), rnot(app("identifierPart"))),
    "var": seq(tok("var"), rnot(app("identifierPart"))),
    "catch": seq(tok("catch"), rnot(app("identifierPart"))),
    "finally": seq(tok("finally"), rnot(app("identifierPart"))),
    "return": seq(tok("return"), rnot(app("identifierPart"))),
    "void": seq(tok("void"), rnot(app("identifierPart"))),
    "continue": seq(tok("continue"), rnot(app("identifierPart"))),
    "for": seq(tok("for"), rnot(app("identifierPart"))),
    "switch": seq(tok("switch"), rnot(app("identifierPart"))),
    "while": seq(tok("while"), rnot(app("identifierPart"))),
    "debugger": seq(tok("debugger"), rnot(app("identifierPart"))),
    "function": seq(tok("function"), rnot(app("identifierPart"))),
    "this": seq(tok("this"), rnot(app("identifierPart"))),
    "with": seq(tok("with"), rnot(app("identifierPart"))),
    "default": seq(tok("default"), rnot(app("identifierPart"))),
    "if": seq(tok("if"), rnot(app("identifierPart"))),
    "throw": seq(tok("throw"), rnot(app("identifierPart"))),
    "delete": seq(tok("delete"), rnot(app("identifierPart"))),
    "in": seq(tok("in"), rnot(app("identifierPart"))),
    "try": seq(tok("try"), rnot(app("identifierPart"))),
    "get": seq(tok("get"), rnot(app("identifierPart"))),
    "set": seq(tok("set"), rnot(app("identifierPart"))),
    "class": seq(tok("class"), rnot(app("identifierPart"))),
    "enum": seq(tok("enum"), rnot(app("identifierPart"))),
    "extends": seq(tok("extends"), rnot(app("identifierPart"))),
    "super": seq(tok("super"), rnot(app("identifierPart"))),
    "const": seq(tok("const"), rnot(app("identifierPart"))),
    "export": seq(tok("export"), rnot(app("identifierPart"))),
    "import": seq(tok("import"), rnot(app("identifierPart"))),
    "implements": seq(tok("implements"), rnot(app("identifierPart"))),
    "let": seq(tok("let"), rnot(app("identifierPart"))),
    "private": seq(tok("private"), rnot(app("identifierPart"))),
    "public": seq(tok("public"), rnot(app("identifierPart"))),
    "interface": seq(tok("interface"), rnot(app("identifierPart"))),
    "package": seq(tok("package"), rnot(app("identifierPart"))),
    "protected": seq(tok("protected"), rnot(app("identifierPart"))),
    "static": seq(tok("static"), rnot(app("identifierPart"))),
    "yield": seq(tok("yield"), rnot(app("identifierPart"))),
    "primaryExpression": choice(seq(app("spaces"), app("this")), seq(app("spaces"), app("identifier")), seq(app("spaces"), app("literal")), app("arrayLiteral"), app("objectLiteral"),
     seq(app("spaces"), tok("("), app("expression"), app("spaces"), tok(")"))),
    "arrayLiteral": choice(seq(app("spaces"), tok("["), app("assignmentExpressionOrElision"), rep(seq(app("spaces"), tok(","), app("assignmentExpressionOrElision"))), app("spaces"), tok("]")),
    seq(app("spaces"), tok("["), app("spaces"), tok("]"))),
    "assignmentExpressionOrElision": choice(app("assignmentExpression"), seq()),
    "objectLiteral": choice(seq(app("spaces"), tok("{"), app("propertyAssignment"), rep(seq(app("spaces"), tok(","), app("propertyAssignment"))), app("spaces"), tok("}")), seq(app("spaces"),
     tok("{"), app("spaces"), tok("}")), seq(app("spaces"), tok("{"), app("propertyAssignment"), rep(seq(app("spaces"), tok(","), app("propertyAssignment"))), app("spaces"), tok(","), app("spaces"),
      tok("}"))),
    "propertyAssignment": choice(seq(app("spaces"), app("get"), app("propertyName"), app("spaces"), tok("("), app("spaces"), tok(")"), app("spaces"), tok("{"), app("functionBody"), app("spaces"),
     tok("}")), seq(app("spaces"), app("set"), app("propertyName"), app("spaces"), tok("("), app("formalParameter"), app("spaces"), tok(")"), app("spaces"), tok("{"), app("functionBody"), app("spaces"), tok("}")), seq(app("propertyName"), app("spaces"), tok(":"), app("assignmentExpression"))),
    "propertyName": choice(seq(app("spaces"), app("identifierName")), seq(app("spaces"), app("stringLiteral")), seq(app("spaces"), app("numericLiteral"))),
    "memberExpression": seq(app("memberExpressionHead"), rep(app("memberExpressionTail"))),
    "memberExpressionHead": choice(seq(app("spaces"), app("new"), app("memberExpression"), app("arguments")), app("primaryExpression"), app("functionExpression")),
    "memberExpressionTail": choice(seq(app("spaces"), tok("["), app("expression"), app("spaces"), tok("]")), seq(app("spaces"), tok("."), app("spaces"), app("identifierName"))),
    "newExpression": choice(app("memberExpression"), seq(app("spaces"), app("new"), app("newExpression"))),
    "callExpression": seq(app("memberExpression"), rep(app("callExpressionTail"))),
    "callExpressionTail": choice(app("arguments"), seq(app("spaces"), tok("["), app("expression"), app("spaces"), tok("]")), seq(app("spaces"), tok("."), app("spaces"), app("identifierName"))),
    "arguments": choice(seq(app("spaces"), tok("("), app("assignmentExpression"), rep(seq(app("spaces"), tok(","), app("assignmentExpression"))), app("spaces"), tok(")")), seq(app("spaces"),
     tok("("), app("spaces"), tok(")"))),
    "leftHandSideExpression": choice(app("callExpression"), app("newExpression")),
    "postfixExpression": choice(seq(app("leftHandSideExpression"), seq(app("spacesNoNL"), tok("++"))), seq(app("leftHandSideExpression"), seq(app("spacesNoNL"), tok("--"))),
     app("leftHandSideExpression")),
    "unaryExpression": choice(seq(app("spaces"), app("delete"), app("unaryExpression")), seq(app("spaces"), app("void"), app("unaryExpression")), seq(app("spaces"), app("typeof"),
    app("unaryExpression")), seq(app("spaces"), tok("++"), app("unaryExpression")), seq(app("spaces"), tok("--"), app("unaryExpression")), seq(app("spaces"), tok("+"), app("unaryExpression")),
     seq(app("spaces"), tok("-"), app("unaryExpression")), seq(app("spaces"), tok("~"), app("unaryExpression")), seq(app("spaces"), tok("!"), app("unaryExpression")), app("postfixExpression")),
    "multiplicativeExpression": choice(seq(app("unaryExpression"), app("spaces"), tok("*"), app("multiplicativeExpression")), seq(app("unaryExpression"), app("spaces"), tok("/"),
    app("multiplicativeExpression")), seq(app("unaryExpression"), app("spaces"), tok("%"), app("multiplicativeExpression")), app("unaryExpression")),
    "additiveExpression": choice(seq(app("multiplicativeExpression"), app("spaces"), tok("+"), app("additiveExpression")), seq(app("multiplicativeExpression"), app("spaces"), tok("-"),
    app("additiveExpression")), app("multiplicativeExpression")),
    "shiftExpression": choice(seq(app("additiveExpression"), app("spaces"), tok("<<"), app("shiftExpression")), seq(app("additiveExpression"), app("spaces"), tok(">>>"), app("shiftExpression")),
     seq(app("additiveExpression"), app("spaces"), tok(">>"), app("shiftExpression")), app("additiveExpression")),
    "relationalExpression": choice(seq(app("shiftExpression"), app("spaces"), tok("<"), app("relationalExpression")), seq(app("shiftExpression"), app("spaces"), tok(">"), app("relationalExpression")),
    seq(app("shiftExpression"), app("spaces"), tok("<="), app("relationalExpression")), seq(app("shiftExpression"), app("spaces"), tok(">="), app("relationalExpression")), seq(app("shiftExpression"),
     app("spaces"), app("instanceof"), app("relationalExpression")), seq(app("shiftExpression"), app("spaces"), app("in"), app("relationalExpression")), app("shiftExpression")),
    "relationalExpressionNoIn": choice(seq(app("shiftExpression"), app("spaces"), tok("<"), app("relationalExpressionNoIn")), seq(app("shiftExpression"), app("spaces"), tok(">"),
    app("relationalExpressionNoIn")), seq(app("shiftExpression"), app("spaces"), tok("<="), app("relationalExpressionNoIn")), seq(app("shiftExpression"), app("spaces"), tok(">="),
    app("relationalExpressionNoIn")), seq(app("shiftExpression"), app("spaces"), app("instanceof"), app("relationalExpressionNoIn")), app("shiftExpression")),
    "equalityExpression": choice(seq(app("relationalExpression"), app("spaces"), tok("=="), app("equalityExpression")), seq(app("relationalExpression"), app("spaces"), tok("!="),
     app("equalityExpression")), seq(app("relationalExpression"), app("spaces"), tok("==="), app("equalityExpression")), seq(app("relationalExpression"), app("spaces"), tok("!=="),
      app("equalityExpression")), app("relationalExpression")),
    "equalityExpressionNoIn": choice(seq(app("relationalExpressionNoIn"), app("spaces"), tok("=="), app("equalityExpressionNoIn")), seq(app("relationalExpressionNoIn"), app("spaces"),
    tok("!="), app("equalityExpressionNoIn")), seq(app("relationalExpressionNoIn"), app("spaces"), tok("==="), app("equalityExpressionNoIn")), seq(app("relationalExpressionNoIn"),
    app("spaces"), tok("!=="), app("equalityExpressionNoIn")), app("relationalExpressionNoIn")),
    "bitwiseANDExpression": choice(seq(app("equalityExpression"), app("spaces"), tok("&"), app("bitwiseANDExpression")), app("equalityExpression")),

    "bitwiseANDExpressionNoIn": choice(seq(app("equalityExpressionNoIn"), app("spaces"), tok("&"), app("bitwiseANDExpressionNoIn")), app("equalityExpressionNoIn")),
    "bitwiseXORExpression": choice(seq(app("bitwiseANDExpression"), app("spaces"), tok("^"), app("bitwiseXORExpression")), app("bitwiseANDExpression")),
    "bitwiseXORExpressionNoIn": choice(seq(app("bitwiseANDExpressionNoIn"), app("spaces"), tok("^"), app("bitwiseXORExpressionNoIn")), app("bitwiseANDExpressionNoIn")),
    "bitwiseORExpression": choice(seq(app("bitwiseXORExpression"), app("spaces"), tok("|"), app("bitwiseORExpression")), app("bitwiseXORExpression")),
    "bitwiseORExpressionNoIn": choice(seq(app("bitwiseXORExpressionNoIn"), app("spaces"), tok("|"), app("bitwiseORExpressionNoIn")), app("bitwiseXORExpressionNoIn")),
    "logicalANDExpression": choice(seq(app("bitwiseORExpression"), app("spaces"), tok("&&"), app("logicalANDExpression")), app("bitwiseORExpression")),
    "logicalANDExpressionNoIn": choice(seq(app("bitwiseORExpressionNoIn"), app("spaces"), tok("&&"), app("logicalANDExpressionNoIn")), app("bitwiseORExpressionNoIn")),
    "logicalORExpression": choice(seq(app("logicalANDExpression"), app("spaces"), tok("||"), app("logicalORExpression")), app("logicalANDExpression")),
    "logicalORExpressionNoIn": choice(seq(app("logicalANDExpressionNoIn"), app("spaces"), tok("||"), app("logicalORExpressionNoIn")), app("logicalANDExpressionNoIn")),
    "conditionalExpression": choice(seq(app("logicalORExpression"), app("spaces"), tok("?"), app("assignmentExpression"), app("spaces"), tok(":"), app("assignmentExpression")),
     app("logicalORExpression")),
    "conditionalExpressionNoIn": choice(seq(app("logicalORExpressionNoIn"), app("spaces"), tok("?"), app("assignmentExpression"), app("spaces"), tok(":"), app("assignmentExpressionNoIn")),
     app("logicalORExpressionNoIn")),
    "assignmentExpression": choice(seq(app("leftHandSideExpression"), app("spaces"), app("assignmentOperator"), app("assignmentExpression")), app("conditionalExpression")),
    "assignmentExpressionNoIn": choice(seq(app("leftHandSideExpression"), app("spaces"), app("assignmentOperator"), app("assignmentExpressionNoIn")), app("conditionalExpressionNoIn")),
    "expression": seq(app("assignmentExpression"), rep(seq(app("spaces"), tok(","), app("assignmentExpression")))),
    "expressionNoIn": seq(app("assignmentExpressionNoIn"), rep(seq(app("spaces"), tok(","), app("assignmentExpressionNoIn")))),
    "assignmentOperator": choice(tok("="), tok(">>>="), tok("<<="), tok(">>="), tok("*="), tok("/="), tok("%="), tok("+="), tok("-="), tok("&="), tok("^="), tok("|=")),
    "statement": choice(app("block"), app("variableStatement"), app("emptyStatement"), app("expressionStatement"), app("ifStatement"), app("iterationStatement"), app("continueStatement"),
    app("breakStatement"), app("returnStatement"), app("withStatement"), app("labelledStatement"), app("switchStatement"), app("throwStatement"), app("tryStatement"), app("debuggerStatement")),
    "block": seq(app("spaces"), tok("{"), app("statementList"), app("spaces"), tok("}")),
    "statementList": rep(app("statement")),
    "variableStatement": seq(app("spaces"), app("var"), app("variableDeclarationList"), app("sc")),
    "variableDeclarationList": seq(app("variableDeclaration"), rep(seq(app("spaces"), tok(","), app("variableDeclaration")))),
    "variableDeclarationListNoIn": seq(app("variableDeclarationNoIn"), rep(seq(app("spaces"), tok(","), app("variableDeclarationNoIn")))),
    "variableDeclaration": seq(app("spaces"), app("identifier"), choice(app("initialiser"), seq())),
    "variableDeclarationNoIn": seq(app("spaces"), app("identifier"), choice(app("initialiserNoIn"), seq())),
    "initialiser": seq(app("spaces"), tok("="), app("assignmentExpression")),
    "initialiserNoIn": seq(app("spaces"), tok("="), app("assignmentExpressionNoIn")),
    "emptyStatement": seq(app("spaces"), tok(";")),
    "expressionStatement": seq(rnot(seq(app("spaces"), choice(tok("{"), app("function")))), app("expression"), app("sc")),
    "ifStatement": seq(app("spaces"), app("if"), app("spaces"), tok("("), app("expression"), app("spaces"), tok(")"), app("statement"), choice(seq(app("spaces"), app("else"),
    app("statement")), seq())),
    "iterationStatement": choice(seq(app("spaces"), app("do"), app("statement"), app("spaces"), app("while"), app("spaces"), tok("("), app("expression"), app("spaces"), tok(")"),
    app("sc")), seq(app("spaces"), app("while"), app("spaces"), tok("("), app("expression"), app("spaces"), tok(")"), app("statement")), seq(app("spaces"), app("for"),
    app("spaces"), tok("("), choice(app("expressionNoIn"), seq()), app("spaces"), tok(";"), choice(app("expression"), seq()), app("spaces"), tok(";"), choice(app("expression"), seq()),
     app("spaces"), tok(")"), app("statement")), seq(app("spaces"), app("for"), app("spaces"), tok("("), app("spaces"), app("var"), app("variableDeclarationListNoIn"), app("spaces"),
     tok(";"), choice(app("expression"), seq()), app("spaces"), tok(";"), choice(app("expression"), seq()), app("spaces"), tok(")"), app("statement")), seq(app("spaces"), app("for"),
     app("spaces"), tok("("), app("leftHandSideExpression"), app("spaces"), app("in"), app("expression"), app("spaces"), tok(")"), app("statement")), seq(app("spaces"), app("for"),
     app("spaces"), tok("("), app("spaces"), app("var"), app("variableDeclarationNoIn"), app("spaces"), app("in"), app("expression"), app("spaces"), tok(")"), app("statement"))),
    "continueStatement": seq(app("spaces"), app("continue"), seq(choice(seq(app("spacesNoNL"), app("identifier")), seq()), app("sc"))),

    "breakStatement": seq(app("spaces"), app("break"), seq(choice(seq(app("spacesNoNL"), app("identifier")), seq()), app("sc"))),
    "returnStatement": seq(app("spaces"), app("return"), choice(seq(seq(app("spacesNoNL"), rnot(app("space"))), app("expression")), seq()), app("sc")),
    "withStatement": seq(app("spaces"), app("with"), app("spaces"), tok("("), app("expression"), app("spaces"), tok(")"), app("statement")),
    "switchStatement": seq(app("spaces"), app("switch"), app("spaces"), tok("("), app("expression"), app("spaces"), tok(")"), app("caseBlock")),
    "caseBlock": choice(seq(app("spaces"), tok("{"), rep(app("caseClause")), app("defaultClause"), rep(app("caseClause")), app("spaces"), tok("}")), seq(app("spaces"), tok("{"),
     rep(app("caseClause")), app("spaces"), tok("}"))),
    "caseClause": seq(app("spaces"), app("case"), app("expression"), app("spaces"), tok(":"), rep(app("statement"))),
    "defaultClause": seq(app("spaces"), app("default"), app("spaces"), tok(":"), rep(app("statement"))),
    "labelledStatement": seq(app("spaces"), app("identifier"), app("spaces"), tok(":"), app("statement")),
    "throwStatement": seq(app("spaces"), app("throw"), app("expression"), app("sc")),
    "tryStatement": choice(seq(app("spaces"), app("try"), app("block"), app("catchBlock"), app("finallyBlock")), seq(app("spaces"), app("try"), app("block"), app("finallyBlock")),
     seq(app("spaces"), app("try"), app("block"), app("catchBlock"))),
    "catchBlock": seq(app("spaces"), app("catch"), app("spaces"), tok("("), app("formalParameter"), app("spaces"), tok(")"), app("block")),
    "finallyBlock": seq(app("spaces"), app("finally"), app("block")),
    "debuggerStatement": seq(app("spaces"), seq(app("debugger"), app("sc"))),
    "functionDeclaration": seq(app("spaces"), app("function"), app("spaces"), app("identifier"), app("spaces"), tok("("), app("formalParameterList"), app("spaces"), tok(")"),
     app("spaces"), tok("{"), app("functionBody"), app("spaces"), tok("}")),
    "functionExpression": choice(seq(app("spaces"), app("function"), app("spaces"), app("identifier"), app("spaces"), tok("("), app("formalParameterList"), app("spaces"), tok(")"),
    app("spaces"), tok("{"), app("functionBody"), app("spaces"), tok("}")), seq(app("spaces"), app("function"), app("spaces"), tok("("), app("formalParameterList"), app("spaces"),
    tok(")"), app("spaces"), tok("{"), app("functionBody"), app("spaces"), tok("}"))),
    "formalParameterList": choice(seq(app("formalParameter"), rep(seq(app("spaces"), tok(","), app("formalParameter")))), seq()),
    "formalParameter": seq(app("spaces"), app("identifier")),
    "functionBody": seq(lookahead(rep(app("directive"))), rep(app("sourceElement"))),
    "sourceElement": choice(app("functionDeclaration"), app("statement")),
    "directive": seq(app("spaces"), app("stringLiteral"), app("sc"))
  }

  let str = """
var str = "HELLO WORLD";
str.charAt(0);            // returns H
"""

  matcher.applyEdit(0, str.high, str)

  pprint matcher.memoTable
