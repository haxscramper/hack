# Spec: https://www.w3.org/TR/css3-selectors/

import std/[xmltree, strutils, strtabs, unicode, math, deques]

const DEBUG = false

type
  ParseError* = object of ValueError

  TokenKind = enum
    tkInvalid

    tkBracketStart, tkBracketEnd
    tkParam
    tkComma

    tkIdentifier, tkString

    tkClass, tkId, tkElement

    tkCombinatorDescendents, tkCombinatorChildren
    tkCombinatorNextSibling, tkCombinatorSiblings

    tkPseudoNthChild, tkPseudoNthLastChild
    tkPseudoNthOfType, tkPseudoNthLastOfType

    tkPseudoFirstOfType, tkPseudoLastOfType
    tkPseudoOnlyChild, tkPseudoOnlyOfType, tkPseudoEmpty
    tkPseudoFirstChild, tkPseudoLastChild

    tkPseudoNot

    tkPredicate

    tkEoi # End of input

  Token = object
    kind: TokenKind
    value: string

const NthKinds = {
  tkPseudoNthChild, tkPseudoNthLastChild,
  tkPseudoNthOfType, tkPseudoNthLastOfType
}

type
  Demand[N] = object
    case kind: Tokenkind
      of tkPredicate:
        predicate: proc(node: N): bool

      of NthKinds:
        a, b: int

      of tkPseudoNot:
        notQuery: QueryPart[N]

      of tkElement:
        element: string

      else:
        discard

  NodeWithParent[N] = object
    parent: N
    index: int
    elementIndex: int

  Combinator = enum
    cmDescendants = tkCombinatorDescendents
    cmChildren = tkCombinatorChildren
    cmNextSibling = tkCombinatorNextSibling
    cmSiblings = tkCombinatorSiblings
    cmLeaf # Special case for the last query

  QueryOption* = enum
    optUniqueIds          ## Assume unique id's or not
    optUnicodeIdentifiers ## Allow non-ascii in identifiers (e.g `#exämple`)
    optSimpleNot          ## Only allow simple selectors as the argument
                          ## for ":not". Combinators and/or commas are not
                          ## allowed even if this option is excluded.

  Query*[N] = object ## Represents a parsed query.
    queries: seq[seq[QueryPart[N]]]
    options: set[QueryOption]

  QueryPart[N] = object
    demands: seq[Demand[N]]
    combinator: Combinator

const
  DefaultQueryOptions* = {optUniqueIds, optUnicodeIdentifiers, optSimpleNot}
  Identifiers = Letters + Digits + {'-', '_', '\\'}
  CssWhitespace = {'\x20', '\x09', '\x0A', '\x0D', '\x0C'}
  Combinators = CssWhitespace + {'+', '~', '>'}

  PseudoNoParamsKinds = {
    tkPseudoFirstOfType, tkPseudoLastOfType,
    tkPseudoOnlyChild, tkPseudoOnlyOfType,
    tkPseudoEmpty, tkPseudoFirstChild,
    tkPseudoLastChild
  }

  PseudoParamsKinds = NthKinds + {tkPseudoNot}

  CombinatorKinds = {
    tkCombinatorChildren, tkCombinatorDescendents,
    tkCombinatorNextSibling, tkCombinatorSiblings
  }

func safeCharCompare(str: string, idx: int, cs: set[char]): bool {.inline.} =
  idx in 0 .. high(str) and str[idx] in cs

func safeCharCompare(str: string, idx: int, c: char): bool {.inline.} =
  return str.safeCharCompare(idx, {c})

func node[N](pair: NodeWithParent[N]): N =
  return pair.parent[pair.index]

func initNotDemand[N](notQuery: QueryPart[N]): Demand[N] =
  result = Demand[N](kind: tkPseudoNot, notQuery: notQuery)

func initElementDemand[N](element: string): Demand[N] =
  result = Demand[N](kind: tkElement, element: element)

func initPseudoDemand[N](kind: TokenKind): Demand[N] =
  result = Demand[N](kind: kind)

func initPredicateDemand[N](
    kind: TokenKind, predicate: proc(node: N): bool): Demand[N] =
  Demand(kind: kind, predicate: predicate)

func initNthChildDemand[N](kind: TokenKind, a, b: int): Demand[N] =
  case kind
    of NthKinds:
      result = Demand[N](kind: kind, a: a, b: b)

    else:
      raiseAssert "invalid kind: " & $kind

func `$`*[N](demand: Demand[N]): string =
  case demand.kind:
    of tkPseudoNot:
      result = ":" & $demand.kind & "(" & $demand.notQuery & ")"

    of NthKinds:
      result = ":" & $demand.kind & "(" & $demand.a & "n, " & $demand.b & ")"

    of PseudoNoParamsKinds:
      result = ":" & $demand.kind

    of tkElement:
      result = demand.element

    else:
      result = $demand.kind

func `==`*(d1, d2: Demand): bool =
  if d1.kind != d2.kind:
    return false

  else:
    case d1.kind:
      of tkPredicate:
        return d1.predicate == d2.predicate

      of NthKinds:
        return d1.a == d2.b

      of tkPseudoNot:
        return d1.notQuery == d2.notQuery

      of tkElement:
        return d1.element == d2.element

      else:
        raise newException(Exception, "Invalid demand kind: " & $d1.kind)

iterator children[N](
    node: N,
    offset: NodeWithParent[N] =
      NodeWithParent[N](parent: nil, index: -1, elementIndex: -1)
  ): NodeWithParent[N] =

  var idx = offset.index + 1
  var elIdx = offset.elementIndex + 1
  while idx < node.len:
    let el = node[idx]
    if el.kind == xnElement:
      yield NodeWithParent[N](
        parent: node, index: idx, elementIndex: elIdx)

      elIdx.inc

    idx.inc

func initToken(kind: TokenKind, value: string = ""): Token =
  return Token(kind: kind, value: value)

func initQueryPart[N](
    demands: seq[Demand[N]], combinator: Combinator): QueryPart[N] =
  return QueryPart[N](demands: demands, combinator: combinator)

func initQuery[N](
    parts: seq[QueryPart[N]],
    options: set[QueryOption] = DefaultQueryOptions): Query[N] =

  Query[N](queries: @[parts], options: options)

func initWithParent*[N](
    parent: N, index: int, elementIndex: int): NodeWithParent[N] =

  NodeWithParent[N](parent: parent, index: index, elementIndex: elementIndex)

func canFindMultiple(q: Querypart, comb: Combinator,
                     options: set[QueryOption]): bool =
  for demand in q.demands:
    if comb in {cmChildren, cmSiblings} and demand.kind in {
      tkPseudoFirstOfType, tkPseudoLastOfType,
      tkPseudoFirstChild, tkPseudoLastChild, tkPseudoOnlyOfType
    }:
      return false

  return true

# func `$`*(q: Query): string =
#   result = q.queryStr

func isValidNotQuery(q: Query, options: set[QueryOption]): bool =
  return
    q.queries.len == 1 and
    q.queries[0].len == 1 and
    (q.queries[0][0].demands.len == 1 or not (optSimpleNot in options))


func initPseudoToken(str: string): Token =
    let kind = case str:
      of ":empty":            tkPseudoEmpty
      of ":only-child":       tkPseudoOnlyChild
      of ":only-of-type":     tkPseudoOnlyOfType
      of ":first-child":      tkPseudoFirstChild
      of ":last-child":       tkPseudoLastChild
      of ":last-of-type":     tkPseudoLastOfType
      of ":first-of-type":    tkPseudoFirstOfType
      of ":not":              tkPseudoNot
      of ":nth-child":        tkPseudoNthChild
      of ":nth-last-child":   tkPseudoNthLastChild
      of ":nth-of-type":      tkPseudoNthOfType
      of ":nth-last-of-type": tkPseudoNthLastOfType
      else:
        raise newException(ParseError, "Unknown pseudo selector: " & str)

    result = initToken(kind)

func isFinishedSimpleSelector(prev: Token, prevPrev: Token): bool =
  if prev.kind in {tkBracketEnd, tkParam, tkElement} + PseudoNoParamsKinds:
    return true

  if prev.kind == tkIdentifier and prevPrev.kind in {tkClass, tkId}:
    return true

func hasAttr[N](node: N, attr: string): bool {.inline.} =
  return not node.attrs.isNil and node.attrs.hasKey(attr)

func validateNth(a, b, nSiblings: int): bool =
    if a == 0:
      return nSiblings == b - 1

    let n = (nSiblings - (b - 1)) / a
    return n.floor == n and n >= 0

proc satisfies(pair: NodeWithParent, demands: seq[Demand]): bool

proc satisfies[N](pair: NodeWithParent[N], demand: Demand[N]): bool =
  let node = pair.node

  case demand.kind:
    of tkPredicate:
      return demand.predicate(node)

    of tkElement:
      return node.tag == demand.element

    of tkPseudoEmpty:
      return node.len == 0

    of tkPseudoOnlyChild:
      for siblingPair in pair.parent.children:
          if siblingPair.node != node:
              return false
      return true

    of tkPseudoOnlyOfType:
      for siblingPair in pair.parent.children:
        if siblingPair.node != node and
            siblingPair.node.tag == node.tag:
          return false
      return true

    of tkPseudoFirstChild:
      return pair.elementIndex == 0

    of tkPseudoLastChild:
      for siblingPair in pair.parent.children(offset = pair):
        return false

      return true

    of tkPseudoFirstOfType:
      for siblingPair in pair.parent.children:
        if siblingPair.node.tag == node.tag:
          return siblingPair.node == node

    of tkPseudoLastOfType:
      for siblingPair in pair.parent.children(offset = pair):
        if siblingPair.node.tag == node.tag:
          return false
      return true

    of tkPseudoNot:
      return not pair.satisfies(demand.notQuery.demands)

    of tkPseudoNthChild:
      return validateNth(demand.a, demand.b, pair.elementIndex)

    of tkPseudoNthLastChild:
      var nSiblingsAfter = 0
      for siblingPair in pair.parent.children(offset = pair):
        nSiblingsAfter.inc
      return validateNth(demand.a, demand.b, nSiblingsAfter)

    of tkPseudoNthOfType:
      var nSiblingsOfTypeBefore = 0
      for siblingPair in pair.parent.children:
        if siblingPair.node == node:
          break

        elif siblingPair.node.tag == node.tag:
          nSiblingsOfTypeBefore.inc

      return validateNth(demand.a, demand.b, nSiblingsOfTypeBefore)

    of tkPseudoNthLastOfType:
      var nSiblingsOfTypeAfter = 0
      for siblingPair in pair.parent.children(offset = pair):
        if siblingPair.node.tag == node.tag:
          nSiblingsOfTypeAfter.inc

        return validateNth(demand.a, demand.b, nSiblingsOfTypeAfter)

    else:
        raiseAssert "Invalid demand: " & $demand

proc satisfies(pair: NodeWithParent, demands: seq[Demand]): bool =
  for demand in demands:
    if not pair.satisfies(demand):
      return false

  return true

iterator searchDescendants(queryPart: QueryPart,
                           position: NodeWithParent): NodeWithParent =
  var queue = initDeque[NodeWithParent]()
  for nodeData in position.node.children:
    queue.addLast(initWithParent(
      position.node, nodeData.index, nodeData.elementIndex))

  while queue.len > 0:
    let pair = queue.popFirst()
    if pair.satisfies queryPart.demands:
      yield pair

    for nodeData in pair.node.children:
      queue.addLast(initWithParent(
        pair.node, nodeData.index, nodeData.elementIndex))

iterator searchChildren(queryPart: QueryPart,
                        position: NodeWithParent): NodeWithParent =
  for pair in position.node.children:
    if pair.satisfies queryPart.demands:
        yield pair

iterator searchSiblings(queryPart: QueryPart,
                        position: NodeWithParent): NodeWithParent =
  for pair in position.parent.children(offset = position):
    if pair.satisfies queryPart.demands:
      yield pair

iterator searchNextSibling(queryPart: QueryPart,
                           position: NodeWithParent): NodeWithParent =
  for pair in position.parent.children(offset = position):
    if pair.satisfies queryPart.demands:
      yield pair
    break

type SearchIterator[N] =
  iterator(q: QueryPart[N], p: NodeWithParent[N]): NodeWithParent[N] {.inline.}

proc exec[N](
    parts: seq[QueryPart],
    root: NodeWithParent[N],
    single: bool,
    options: set[QueryOption],
    result: var seq[N]
  ) =

  var combinator = cmDescendants
  var buffer = initDeque[NodeWithParent[N]]()
  var partIndex = 0
  buffer.addLast root

  template search(position: NodeWithParent[N], itr: SearchIterator) =
    for next in itr(parts[partIndex], position):
      if partIndex == high(parts):
        result.add next.node
        if single:
          return

      else:
        buffer.addLast next

      if not parts[partIndex].canFindMultiple(combinator, options):
        break

  while buffer.len > 0:
    for _ in 0..<buffer.len:
      let position = buffer.popFirst
      case combinator
        of cmDescendants: search(position, searchDescendants)
        of cmChildren:    search(position, searchChildren)
        of cmSiblings:    search(position, searchSiblings)
        of cmNextSibling: search(position, searchNextSibling)
        of cmLeaf: discard

    combinator = parts[partIndex].combinator
    partIndex.inc

proc exec*[N](
    query: Query,
    root: N,
    single: bool,
    wrapperRoot: N
  ): seq[N] =

  result = newSeq[N]()
  let wRoot = NodeWithParent[N](parent: wrapperRoot, index: 0, elementIndex: 0)
  for parts in query.queries:
    parts.exec(wRoot, single, query.options, result)

import std/[htmlparser, streams]

when isMainModule:
  let html = """
<!DOCTYPE html>
<html>
  <head><title>Example</title></head>
  <body>
    <p>1</p>
    <p>2</p>
    <p>3</p>
    <p>4</p>
  </body>
</html>
"""

  let xml = parseHtml(newStringStream(html))

  var query = initQuery(@[
    initQueryPart(@[
      initElementDemand[XmlNode]("p"),
      initNthChildDemand[XmlNode](tkPseudoNthChild, 2, 1)
    ], cmLeaf)
  ])

  let wrapper = <>wrapper(xml)
  let wrapperRoot = <>"wrapper-root"(wrapper)

  let res = query.exec(wrapper, false, wrapperRoot)
  echo res
  echo "ok"
