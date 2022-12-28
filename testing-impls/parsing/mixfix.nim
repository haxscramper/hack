import std/[sequtils, tables, strutils, algorithm]
import fusion/matching

type
  Associativity = enum
    aLeft
    aRight
    aNon

  FixityKind = enum
    fPrefix
    fInfix
    fPostfix
    fClosed

  Fixity = object
    kind*: FixityKind
    assoc*: Associativity

  NamePart = string
  OperatorPattern = object
    parts*: seq[seq[NamePart]]

  Operator = object
    fixity*: Fixity
    pattern*: OperatorPattern

  Expr = object
    operator*: Operator
    args*: seq[Expr]

  Graph = ref object
    table: Table[int, seq[Operator]]

  Expr1 = object
    g: Graph

  Precs = object
    g: Graph
    values: seq[int]

  Prec = object
    g: Graph
    value: int

  Closed = object
    g: Graph
    value: int

  NonAssoc = object
    g: Graph
    value: int

  PreRight = object
    g: Graph
    value: int

  PostLeft = object
    g: Graph
    value: int

  Inner = object
    g: Graph
    value: int
    fixity: Fixity

  Backbone = object
    g: Graph
    op: Operator

  Seq[T] = object
    elements: seq[T]

  Opts[T] = object
    opts: seq[T]

  Plus[T] = object
    plus: T

  Between[A, B] = object
    head: A
    rest: seq[B]

proc arity(operator: Operator): int = operator.pattern.parts.len()

proc newExpr(operator: Operator, args: seq[Expr]): Expr =
    Expr(operator: operator, args: args)

proc wellFormed(self: Expr): bool =
  self.args.len() == self.operator.arity and
  self.args.all(wellFormed)

proc ops(self: Graph, prec: int, fix: Fixity): seq[Operator] =
  if self.table.matches({prec : @ops}):
    result = ops.filterIt(it.fixity == fix)

proc succ(self: Graph, prec: int): seq[int] =
  for key, _ in self.table:
    if key > prec:
      result.add key

proc all(self: Graph): seq[int] = toSeq(self.table.keys())

proc backbone(self: OperatorPattern): seq[seq[NamePart]] =
  var first = -1
  for i in 0 ..< self.parts.len:
    if self.parts[i].len > 0:
      first = i

  var last = -1
  for i in countdown(self.parts.len, 0):
    if self.parts[i].len > 0:
      last = i

  if first > 0 and last > 0:
    return self.parts[first .. last]

proc toOperatorPattern(str: string): OperatorPattern =
  for part in str.split('_'):
    result.parts.add @[]
    for s in part.split(' '):
      result.parts[^1].add s

type
  ParseInput = seq[string]
  ParseResult[T] = object
    case ok*: bool
      of true:
        rest*: ParseInput
        value*: T

      of false:
        fail*: ParseError

  ParseErrorKind = enum
    UnexpectedToken
    UnexpectedEndOfInput
    UnparsedInput
    EmptyOpts

  ParseError = ref object of CatchableError
    case kind*: ParseErrorKind
      of UnexpectedToken:
        token*: string

      of UnparsedInput:
        input*: ParseInput

      else:
        discard


  Parser = object

  Token = object
    str: string

proc parse(self: Parser, toks: ParseInput): ParseResult =
  discard

proc parse(self: Token, toks: ParseInput): ParseResult[Token] =
  if toks.len == 0:
    return ParseResult[Token](
      ok: false,
      fail: ParseError(kind: UnexpectedEndOfInput)
    )

  else:
    if self.str == toks[0]:
      return ParseResult[Token](ok: true, rest: toks[1..^1])

    else:
      return ParseResult[Token](
        ok: false,
        fail: ParseError(kind: UnexpectedToken, token: toks[0])
      )

proc parse[A, B](self: (A, B), toks: ParseInput): auto =
  let result1 = parse(self[0], toks)
  let result2 = parse(self[1], result1.rest)
  return ParseResult[(typeof(result1.value), typeof(result2.value))](
    ok: true,
    rest: result2.rest,
    value: (result1.value, result2.value)
  )


proc parse[A, B, C](self: (A, B, C), toks: ParseInput): ParseResult[(A, B, C)] =
  let result1 = parse(self[0], toks)
  let result2 = parse(self[1], result1.rest)
  let result3 = parse(self[2], result2.rest)
  return ParseResult[(A, B, C)](
    rest: result2.rest,
    value: (result1.value, result2.value, result3.value)
  )

type
  Opt[A, B] = object
    first*: A
    second*: B

  Either[A, B] = object
    case isFirst*: bool
      of true:
        opt1*: A

      of false:
        opt2*: B

proc parse[A, B](self: Opt[A, B], toks: ParseInput): ParseResult[Either[A, B]] =
  let match1 = parse(self.first, toks)
  let match2 = parse(self.second, toks)

  if match1.ok and not match2.ok:
    return ParseResult[Either[A, B]](
      ok: true, value: Either[A, B](isFirst: true, opt1: match1.value))

  elif not match1.ok and match2.ok:
    return ParseResult[Either[A, B]](
      ok: true, value: Either[A, B](isFist: false, opt2: match2.value)
    )

  elif match1.ok and match2.ok:
    if match1.len < match2.len:
      return ParseResult[Either[A, B]](
        ok: true, value: Either[A, B](isFirst: true, opt1: match1.value)
      )

    else:
      return ParseResult[Either[A, B]](
        ok: true, value: Either[A, B](isFirst: false, opt1: match2.value)
      )

  else:
    return ParseResult[Either[A, B]](
      ok: false, fail: match1.fail
    )

proc parse[T](self: Seq[T], toks: var ParseInput): ParseResult[seq[T]] =
  result = ParseResult[seq[T]](ok: true)
  for item in self.elements:
    let match = parse(toks)
    toks = match.rest
    result.value.add match.value

proc parse[T](self: Opts[T], toks: ParseInput): auto =
  var oks, errs: seq[typeof(parse(self.opts[0], toks))]
  for opt in self.opts:
    let match = parse(opt, toks)
    if match.ok:
      oks.add match

    else:
      errs.add match


  if oks.len > 0:
    oks = sortedByIt(oks, it.rest.len())
    return oks[0]

  else:
    if errs.len > 0:
      return errs[0]

    else:
      return ParseResult[typeof(oks[0].value)](
        ok: false, fail: ParseError(kind: EmptyOpts))

proc parse[T](self: Plus[T], toks: ParseInput): ParseResult[seq[T]] =
  var match = parse(self.plus, toks)
  if not match.ok:
    return ParseResult[seq[T]](ok: false, fail: match.fail)

  else:
    var res: seq[T] = @[match.value]
    while match.ok:
      match = parse(self.plus, toks)
      if match.ok:
        toks = match.rest
        res.add match.value

    return ParseResult[seq[T]](ok: true, value: res)


proc parse[A, B](self: Between[A, B], toks: ParseInput): ParseResult[seq[A]] =
  var match = parse(self.head, toks)
  if match.ok:
    discard
        # if let Some((first, rest)) = self.1.split_first() {
        #     (first, Seq(rest.iter().map(|p| (&self.0, p)).collect()))
        #         .p(toks)
        #         .map(|(toks, (_, xs))| (toks, xs.into_iter().map(|(x, _)| x).collect()))
        # } else {
        #     Ok((toks, vec![]))
        # }

  else:
    return ParseResult[seq[A]](ok: true)

proc parse(self: Prec, toks: ParseInput): ParseResult[Expr] =
  let opts = Opts[Fixity](opts: @[
    Fixity(kind: fClosed, )
    # vec![
    #     &Closed(self.0, self.1),
    #     &NonAssoc(self.0, self.1),
    #     &PreRight(self.0, self.1),
    #     &PostLeft(self.0, self.1),
    # ]
  ])

  # parse(opts, toks)

proc parse(self: Precs, toks: ParseInput): ParseResult[Expr] =
  # Parse any of the alternatives from `Precs`, return longest one
  let opts = Opts[Prec](opts: self.values.mapIt(Prec(g: self.g, value: it)))
  parse(opts, toks)

proc parse(self: Expr1, toks: ParseInput): ParseResult[Expr] =
  #
  parse(Precs(g: self.g, values: self.g.all()), toks)


proc parse(self: Inner, toks: ParseInput): ParseResult[Expr]
proc parse(self: Closed, toks: ParseInput): ParseResult[Expr] =
  parse(Inner(g: self.g, value: self.value, fixity: Fixity(kind: fClosed)), toks)

proc parse(self: NonAssoc, toks: ParseInput): ParseResult[Expr] =
  let succ = Precs(g: self.g, values: self.g.succ(self.value))
  let match1 = parse(succ, toks)
  let match2 = parse(
    Inner(g: self.g, value: self.value, fixity: Fixity(kind: fInfix, assoc: aNon)),
    toks
  )
  var expr = match2.value
  let match3 = parse(succ, toks)

  expr.args.insert(match1.value, 0)
  expr.args.add match3.value

  return ParseResult[Expr](ok: true, value: expr, rest: toks)

proc parse(self: PreRight, toks: ParseInput): ParseResult[Expr] =
  let succ = Precs(g: self.g, values: self.g.succ(self.value));
  let first = Inner(g: self.g, value: self.value, fixity: Fixity(kind: fPrefix))
  let second = (succ, Inner(g: self.g, value: self.value,
                            fixity: Fixity(kind: fInfix, assoc: aRight)))
  let match1 = parse((
    Plus[Opt[Inner, (Precs, Inner)]](
      plus: Opt[Inner, (Precs, Inner)](
        first: first, second: second)), succ), toks)

  var
    toks = match1.rest
    inners: seq[Either[Expr, (Expr, Expr)]]  = match1.value[0]
    last: Expr = match1.value[1]

  var mappedExprs: seq[Expr]
  for inner in inners:
    if inner.isFirst:
      mappedExprs.add inner.opt1

    else:
      var rest: Expr = inner.opt2[1]
      rest.args.add inner.opt2[0]
      mappedExprs.add rest

  reverse(mappedExprs)
  var resultExpr: Expr = mappedExprs[0]
  for item in mappedExprs[1..^1]:
    resultExpr.args.add item

  resultExpr.args.add last

  return ParseResult[Expr](ok: true, rest: toks, value: resultExpr)

proc parse(self: PostLeft, g: ParseInput): ParseResult[Expr] =
  let succ = Precs(g: self.g, values: self.g.succ(self.value));
  let match1 = parse((
    Plus(plus: Opt(
      Inner(g: self.g, value: self.value, Fixity(kind: fPostfix)),
      (Inner(g: self.g, value: self.value, Fixity(kind: fInfix, assoc: aLeft)), succ)
    )),
    succ), toks)

  var
    toks: ParseInput = match1.rest
    inners: seq[Either[(Expr, Expr), Expr]]  = match1.value[0]
    last: Expr = match1.value[1]

  var mappedExprs: seq[Expr]
  for inner in inners:
    if not inner.isFirst:
      mappedExprs.add inner.opt2

    else:
      var rest: Expr = inner.opt1[1]
      rest.args.add inner.opt1[0]
      mappedExprs.add rest

  reverse(mappedExprs)
  var resultExpr: Expr = mappedExprs[0]
  for item in mappedExprs[1..^1]:
    resultExpr.args.add item

  resultExpr.args.add last

  return ParseResult[Expr](ok: true, rest: toks, value: resultExpr)

proc parse(self: Inner, toks: ParseInput): ParseResult[Expr] =
  var bones: Backbone
  for op in self.g.ops(self.value, self.fixity):
    bones.add Backbone(self.g, op)

  return parse(bones, toks)

proc parse(self: Backbone, toks: ParseInput): ParseResult[Expr] =
  var between = Between[Expr1, Seq[Token]](head: Expr1(g: self.g))
  for bone in self.op.backbone():
    var tokens: seq[Token]
    for token in bone:
      tokens.add Token(str: token)

    between.rest.add Seq[Token](elements: tokens)

  let match: ParseResult[seq[Expr]] = parse(between, toks)

  return ParseResult(
    ok: true, rest: match.rest,
    value: Expr(operator: self.op, args: match.value))

proc parseExpr(graph: Graph, tokens: ParseInput): Expr =
  let match: ParseResult[Expr] = parse(Expr1(g: graph), tokens)
  return match.value



