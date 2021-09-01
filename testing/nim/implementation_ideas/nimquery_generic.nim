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

      tkAttributeExact     # [attr=...]
      tkAttributeItem      # [attr~=...]
      tkAttributePipe      # [attr|=...]
      tkAttributeExists    # [attr]
      tkAttributeStart     # [attr^=...]
      tkAttributeEnd       # [attr$=...]
      tkAttributeSubstring # [attr*=...]

      tkPseudoNthChild, tkPseudoNthLastChild
      tkPseudoNthOfType, tkPseudoNthLastOfType

      tkPseudoFirstOfType, tkPseudoLastOfType
      tkPseudoOnlyChild, tkPseudoOnlyOfType, tkPseudoEmpty
      tkPseudoFirstChild, tkPseudoLastChild

      tkPseudoNot

      tkEoi # End of input

    Token = object
      kind: TokenKind
      value: string

const AttributeKinds = {
  tkAttributeExact, tkAttributeItem,
  tkAttributePipe, tkAttributeExists,
  tkAttributeStart, tkAttributeEnd,
  tkAttributeSubstring
}

const NthKinds = {
  tkPseudoNthChild, tkPseudoNthLastChild,
  tkPseudoNthOfType, tkPseudoNthLastOfType
}

type
  Demand = object
    case kind: Tokenkind
      of AttributeKinds:
        attrName, attrValue: string

      of NthKinds:
        a, b: int

      of tkPseudoNot:
        notQuery: QueryPart

      of tkElement:
        element: string

      else:
        discard

  NodeWithParent = tuple
    parent: XmlNode
    index, elementIndex: int

  Combinator = enum
    cmDescendants = tkCombinatorDescendents,
    cmChildren = tkCombinatorChildren,
    cmNextSibling = tkCombinatorNextSibling,
    cmSiblings = tkCombinatorSiblings,
    cmLeaf # Special case for the last query

  QueryOption* = enum
    optUniqueIds          ## Assume unique id's or not
    optUnicodeIdentifiers ## Allow non-ascii in identifiers (e.g `#exämple`)
    optSimpleNot          ## Only allow simple selectors as the argument
                          ## for ":not". Combinators and/or commas are not
                          ## allowed even if this option is excluded.

  Lexer = object
    input: string
    pos: int
    options: set[QueryOption]
    current, next: Token

  Query* = object ## Represents a parsed query.
    queries: seq[seq[QueryPart]]
    options: set[QueryOption]

  QueryPart = object
    demands: seq[Demand]
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

func node(pair: NodeWithParent): XmlNode =
  return pair.parent[pair.index]

func attrComparerString(kind: TokenKind): string =
  case kind:
    of tkAttributeExact: return "="
    of tkAttributeItem: return "~="
    of tkAttributePipe: return "|="
    of tkAttributeExists: return ""
    of tkAttributeStart: return "^="
    of tkAttributeEnd: return "$="
    of tkAttributeSubstring: return "*="
    else: raiseAssert "Invalid attr kind: " & $kind

func newUnexpectedCharacterException(s: string): ref ParseError =
  return newException(ParseError, "Unexpected character: '" & s & "'")

func newUnexpectedCharacterException(c: char): ref ParseError =
  newUnexpectedCharacterException($c)

func initNotDemand(notQuery: QueryPart): Demand =
  result = Demand(kind: tkPseudoNot, notQuery: notQuery)

func initElementDemand(element: string): Demand =
  result = Demand(kind: tkElement, element: element)

func initPseudoDemand(kind: TokenKind): Demand =
  result = Demand(kind: kind)

func initAttributeDemand(kind: TokenKind, name, value: string): Demand =
  case kind
    of AttributeKinds:
      result = Demand(kind: kind, attrName: name, attrValue: value)

    else:
      raiseAssert "invalid kind: " & $kind

func initNthChildDemand(kind: TokenKind, a, b: int): Demand =
  case kind
    of NthKinds:
      result = Demand(kind: kind, a: a, b: b)

    else:
      raiseAssert "invalid kind: " & $kind

func `$`*(demand: Demand): string =
  case demand.kind:
    of AttributeKinds:
      if demand.kind == tkAttributeExists:
        result = "[" & demand.attrName & "]"

      else:
        result = "[" & demand.attrName & demand.kind.attrComparerString &
          "'" & demand.attrValue & "']"

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
      of AttributeKinds:
        return d1.attrName == d2.attrName and d1.attrValue == d2.attrValue

      of NthKinds:
        return d1.a == d2.b

      of tkPseudoNot:
        return d1.notQuery == d2.notQuery

      of tkElement:
        return d1.element == d2.element

      else:
        raise newException(Exception, "Invalid demand kind: " & $d1.kind)

iterator children(node: XmlNode,
                  offset: NodeWithParent = (nil, -1, -1)): NodeWithParent =
  var idx = offset.index + 1
  var elIdx = offset.elementIndex + 1
  while idx < node.len:
    let el = node[idx]
    if el.kind == xnElement:
      yield (parent: node, index: idx, elementIndex: elIdx).NodeWithParent
      elIdx.inc

    idx.inc

func initToken(kind: TokenKind, value: string = ""): Token =
  return Token(kind: kind, value: value)

func initQueryPart(demands: seq[Demand], combinator: Combinator): QueryPart =
  return QueryPart(demands: demands, combinator: combinator)

func initQuery(
    parts: seq[QueryPart],
    options: set[QueryOption] = DefaultQueryOptions): Query =
  Query(queries: @[parts], options: options)


func canFindMultiple(q: Querypart, comb: Combinator,
                     options: set[QueryOption]): bool =
  for demand in q.demands:
    if optUniqueIds in options and
       demand.kind in AttributeKinds and
       demand.attrName == "id":

      return false

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

func hasAttr(node: XmlNode, attr: string): bool {.inline.} =
  return not node.attrs.isNil and node.attrs.hasKey(attr)

func validateNth(a, b, nSiblings: int): bool =
    if a == 0:
      return nSiblings == b - 1

    let n = (nSiblings - (b - 1)) / a
    return n.floor == n and n >= 0

func satisfies(pair: NodeWithParent, demands: seq[Demand]): bool
               {.raises: [], gcsafe.}

func satisfies(pair: NodeWithParent, demand: Demand): bool =
    let node = pair.node

    case demand.kind:
      of tkAttributeExists:
        return node.hasAttr(demand.attrName)

      of tkAttributeItem:
        return node.hasAttr(demand.attrName) and
          (demand.attrValue.len > 0) and
          demand.attrValue in node.attr(demand.attrName).split(CssWhitespace)

      of tkAttributePipe:
        return node.hasAttr(demand.attrName) and
          demand.attrValue == node.attr(demand.attrName).split("-")[0]

      of tkAttributeExact:
        return node.attr(demand.attrName) == demand.attrValue

      of tkAttributeStart:
        return demand.attrValue.len > 0 and
          node.attr(demand.attrName).startsWith(demand.attrValue)

      of tkAttributeEnd:
        return demand.attrValue.len > 0 and
          node.attr(demand.attrName).endsWith(demand.attrValue)

      of tkAttributeSubstring:
        return demand.attrValue.len > 0 and
          node.attr(demand.attrName) in demand.attrValue

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

func satisfies(pair: NodeWithParent, demands: seq[Demand]): bool =
  for demand in demands:
    if not pair.satisfies(demand):
      return false

  return true

iterator searchDescendants(queryPart: QueryPart,
                           position: NodeWithParent): NodeWithParent =
  var queue = initDeque[NodeWithParent]()
  for nodeData in position.node.children:
    queue.addLast((parent: position.node, index: nodeData.index,
      elementIndex: nodeData.elementIndex))

  while queue.len > 0:
    let pair = queue.popFirst()
    if pair.satisfies queryPart.demands:
      yield pair

    for nodeData in pair.node.children:
      queue.addLast((parent: pair.node, index: nodeData.index,
        elementIndex: nodeData.elementIndex))

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

type SearchIterator = iterator(q: QueryPart,
                               p: NodeWithParent): NodeWithParent {.inline.}

func exec(parts: seq[QueryPart],
          root: NodeWithParent,
          single: bool,
          options: set[QueryOption],
          result: var seq[XmlNode]) =
  var combinator = cmDescendants
  var buffer = initDeque[NodeWithParent]()
  var partIndex = 0
  buffer.addLast root

  template search(position: NodeWithParent, itr: SearchIterator) =
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

func exec*(query: Query, root: XmlNode, single: bool): seq[XmlNode]
           {.raises: [].} =

  result = newSeq[XmlNode]()

  let wrapper = <>wrapper(root)
  let wRoot = (parent: <>"wrapper-root"(wrapper), index: 0, elementIndex: 0)
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
      initElementDemand("p"),
      initNthChildDemand(tkPseudoNthChild, 2, 1)
    ], cmLeaf)
  ])

  let res = query.exec(xml, single = false)
  echo res
  echo "ok"
