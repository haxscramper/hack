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

proc parse[A, B](self: (A, B), toks: ParseInput): ParseResult[(A, B)] =
  let result1 = parse(self[0], toks)
  let result2 = parse(self[1], result1.rest)
  return ParseResult(
    rest: result2.rest,
    value: (result1.value, result2.value)
  )


proc parse[A, B, C](self: (A, B, C), toks: ParseInput): ParseResult[(A, B, C)] =
  let result1 = parse(self[0], toks)
  let result2 = parse(self[1], result1.rest)
  let result3 = parse(self[2], result2.rest)
  return ParseResult(
    rest: result2.rest,
    value: (result1.value, result2.value, result3.value)
  )

type
  Choice[A, B] = object
    alt1*: A
    alt2*: B

  Either[A, B] = object
    case isFirst*: bool
      of true:
        opt1*: A

      of false:
        opt2*: B

proc parse[A, B](self: Choice[A, B], toks: ParseInput): ParseResult[Either[A, B]] =
  let match1 = parse(self.alt1, toks)
  let match2 = parse(self.alt2, toks)

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

type
  Seq[T] = object
    elements: seq[T]

proc parse[T](self: Seq[T], toks: var ParseInput): ParseResult[seq[T]] =
  result = ParseResult[seq[T]](ok: true)
  for item in self.elements:
    let match = parse(toks)
    toks = match.rest
    result.value.add match.value

type
  Opts[T] = object
    opts: seq[T]

proc parse[T](self: Opts[T], toks: ParseInput): ParseResult[T] =
  var oks, errs: seq[ParseResult[T]]
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
      return ParseResult[T](ok: false, fail: ParseError(kind: EmptyOpts))

type
  Plus[T] = object
    plus: T

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


type
  Between[A, B] = object
    head: A
    rest: seq[B]

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

type
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
    g: Graph,
    op: Operator


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

  parse(opts, toks)

proc parse(self: Precs, toks: ParseInput): ParseResult[Expr] =
  let opts = Opts[Prec](opts: self.values.mapIt(Prec(g: self.g, value: it)))
  parse(opts, toks)

proc parse(self: Expr1, toks: ParseInput): ParseResult[Expr] =
  parse(Precs(g: self.g, values: self.g.all()), toks)

proc parse(self: Closed, toks: ParseInput): ParseResult[Expr] =
  Inner(self.0, self.1, Fixity::Closed).p(toks)

proc parse(self: NonAssoc, toks: ParseInput): ParseResult[Expr] =
  let succ = Precs(g: self.g, values: self.g.succ(self.value));
  let (toks, left) = succ.p(toks)?;
  let (toks, mut expr) = Inner(self.0, self.1, Fixity::Infix(Associativity::Non)).p(toks)?;
  let (toks, right) = succ.p(toks)?;
  expr.args.insert(0, left);
  expr.args.push(right);
  Ok((toks, expr))

proc parse(self: PreRight, toks: ParseInput): ParseResult[Expr] = 
  let succ = Precs(self.0, self.0.succ(self.1));
  let (toks, (inners, last)) = (
      Plus(Opt(
          Inner(self.0, self.1, Fixity::Prefix),
          (
              &succ,
              Inner(self.0, self.1, Fixity::Infix(Associativity::Right)),
          ),
      )),
      &succ,
  )
      .p(toks)?;

  let mut expr = inners
      .into_iter()
      .map(|e| {
          e.map_right(|(first, mut rest)| {
              rest.args.insert(0, first);
              rest
          })
          .into_inner()
      })
      .rev()
      .fold1(|right, mut left| {
          left.args.push(right);
          left
      })
      .unwrap();

  expr.args.push(last);

  Ok((toks, expr))

proc parse(self: PostLeft, g: ParseInput): ParseResult[Expr] = 
  let succ = Precs(self.0, self.0.succ(self.1));

  let (toks, (first, inners)) = (
      &succ,
      Plus(Opt(
          Inner(self.0, self.1, Fixity::Postfix),
          (
              Inner(self.0, self.1, Fixity::Infix(Associativity::Left)),
              &succ,
          ),
      )),
  )
      .p(toks)?;

  let mut expr = inners
      .into_iter()
      .map(|e| {
          e.map_right(|(mut rest, last)| {
              rest.args.push(last);
              rest
          })
          .into_inner()
      })
      .fold1(|left, mut right| {
          right.args.insert(0, left);
          right
      })
      .unwrap();

  expr.args.insert(0, first);

  Ok((toks, expr))

proc parse(self: Inner, toks: ParseInput): ParseResult[Expr] =
    Opts(
        self.0
            .ops(self.1, self.2)
            .into_iter()
            .map(|o| Backbone(self.0, o))
            .collect(),
    )
    .p(toks)

proc parse(self: Backbone, toks: ParseInput): ParseResult[Expr] = 
  let (toks, exprs) = Between(
      Expr_(self.0),
      self.1
          .pattern
          .backbone()
          .iter()
          .map(|b| Seq(b.iter().map(|t| Tok(t)).collect()))
          .collect(),
  )
  .p(toks)?;

  Ok((toks, Expr::new(self.1.clone(), exprs)))

proc parseExpr(graph: Graph, tokens: ParseInput): Expr =
  let (unparsed, expr) = Expr_(graph).p(tokens)?;



#[cfg(test)]
mod tests {
    use super::*;
    use petgraph::graph::DiGraph;
    use std::collections::HashMap;

    #[test]
    fn test_tok() {
        let input = [":"];
        let (toks, ()) = Tok(":").p(&input).unwrap();
        assert_eq!(toks.len(), 0);
    }

    fn simple_graph() -> impl PrecedenceGraph {
        let atom = Operator {
            fixity: Fixity::Closed,
            pattern: "•".into(),
        };
        let plus = Operator {
            fixity: Fixity::Infix(Associativity::Left),
            pattern: "_+_".into(),
        };
        let well_typed = Operator {
            fixity: Fixity::Postfix,
            pattern: "_⊢_:".into(),
        };
        let mut g = DiGraph::new();
        let a = g.add_node(vec![atom]);
        let pl = g.add_node(vec![plus]);
        let wt = g.add_node(vec![well_typed.clone()]);
        g.add_edge(pl, a, ());
        g.add_edge(wt, a, ());
        g.add_edge(wt, pl, ());
        g
    }

    #[test]
    fn test_simple_parse() {
        let input: Vec<_> = "•+•⊢•:".chars().map(|c| c.to_string()).collect();
        let input: Vec<_> = input.iter().map(|i| i.as_str()).collect();
        println!("{:?}", input);
        let expr = parse_expr(&simple_graph(), &input).unwrap();
        println!("{:#?}", expr);
        assert!(expr.well_formed());
    }

    #[test]
    fn test_unexpected_token() {
        let input = vec!["abc"];
        let err = parse_expr(&simple_graph(), &input).unwrap_err();
        assert_eq!(ParseError::UnexpectedToken("abc"), err);
    }

    fn apply_graph() -> impl PrecedenceGraph {
        let mut g = HashMap::new();
        let a = Operator {
            fixity: Fixity::Closed,
            pattern: "a".into(),
        };
        let b = Operator {
            fixity: Fixity::Closed,
            pattern: "b".into(),
        };
        let app = Operator {
            fixity: Fixity::Infix(Associativity::Left),
            pattern: "_ _".into(),
        };
        g.insert(1, vec![a, b]);
        g.insert(0, vec![app]);
        g
    }

    #[test]
    fn test_apply() {
        let input = vec!["a", "b"];
        let expr = parse_expr(&apply_graph(), &input).unwrap();
        println!("{:#?}", expr);
    }
}
