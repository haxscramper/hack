import std/[algorithm]
import hmisc/hdebug_misc

proc re2post(inRe: string): string =
  var
    nalt: int
    natom: int
    dst: int

  var paren: array[100, tuple[nalt, natom: int]]
  var p: int

  for idx, re in pairs(inRe):
    case re:
      of '(':
        if (natom > 1):
          dec natom
          result.add '.'

        # if (p >= paren + 100) {return NULL;}

        paren[p].nalt  = nalt
        paren[p].natom = natom
        inc p
        nalt  = 0
        natom = 0

      of '|':
        # if (natom == 0) {return NULL;} # FIXME

        while ((dec natom; natom) > 0):
          result.add '.'

        inc nalt

      of ')':
        # if (p == paren) {return NULL;}
        # if (natom == 0) {return NULL;}

        while ((dec natom; natom) > 0):
          result.add '.'

        while nalt > 0:
          result.add '|'
          dec nalt

        dec p

        nalt  = paren[p].nalt
        natom = paren[p].natom
        inc natom

      of '*', '+', '?':
        # if (natom == 0) {return NULL;}
        result.add re

      else:
        if (natom > 1):
          dec natom;
          result.add '.'

        result.add re
        inc natom

    # if (p != paren) {return NULL;}

    while natom > 0:
        result.add '.'
        dec natom

    while nalt > 0:
      result.add '|'
      dec nalt

type
  StateKind = enum
    skValue
    skMatch
    skSplit

  State = ref object
    lastlist: int
    value: char
    out0: State
    out1: State
    kind: StateKind

  Frag = ref object
    start: State
    outList: PtrList

  PtrList {.union.} = ref object
    next: PtrList
    state: State

  List = ref seq[State]
  DState = ref object
    l: List
    next: array[char, DState]
    left, right: int




var nstate: int

proc newState(out0, out1: State): State =
  result = State(kind: skSplit, out0: out0, out1: out1, lastlist: 0)
  inc nstate

proc newState(ch: char): State =
  result = State(kind: skValue, value: ch)
  inc nstate

proc newFrag(start: State, outList: Ptrlist): Frag =
  Frag(start: start, outList: outList)

proc newList(state: State): PtrList =
  PtrList(next: nil, state: state)

proc patch(l: Ptrlist, s: State) =
  var next: PtrList
  var l = l
  while not isNil(l):
    next = l.next
    l.state = s
    l = next

proc append(l1, l2: Ptrlist): Ptrlist =
  var oldl1 = l1
  var l1 = l1

  while not isNil(l1.next):
    l1 = l1.next

  l1.next = l2
  return oldl1

proc post2nfa(postfix: string): State =
  var stack: seq[Frag]
  # char*  p;
  var e1, e2, e: Frag
  var s: State

  for p in postfix:
    case p:
      of '.':
        e2 = stack.pop()
        e1 = stack.pop()
        patch(e1.outList, e2.start)
        stack.add(newFrag(e1.start, e2.outList))

      of '|':
        e2 = stack.pop()
        e1 = stack.pop()
        s  = newState(e1.start, e2.start)
        stack.add(newFrag(s, append(e1.outList, e2.outList)))

      of '?':
        e = stack.pop()
        s = newState(e.start, nil)
        stack.add(newFrag(s, append(e.outList, newList(s.out1))))

      of '*':
        e = stack.pop()
        s = newState(e.start, nil)
        patch(e.outList, s)
        stack.add(newFrag(s, newList(s.out1)))

      of '+':
        e = stack.pop()
        s = newState(e.start, nil)
        patch(e.outList, s)
        stack.add(newFrag(e.start, newList(s.out1)))

      else:
        s = newState(p)
        stack.add(newFrag(s, newList(s.out0)))



    e = stack.pop()
    # if (stackp != stack):
    #     return nil

    patch(e.outList, State(kind: skMatch));
    return e.start

var l1, l2: List
var listid: int

iterator items(list: List): auto =
  for item in list[]:
    yield item

proc add(list: List, item: State) = list[].add item
proc len(list: List): int = list[].len

proc ismatch(l: List): bool =
  for state in l:
    if state.kind == skMatch:
      return true

  return false

proc addstate(l: var List, s: State) =
    if (isNil(s) or s.lastlist == listid):
      return

    s.lastlist = listid
    if (s.kind == skSplit):
        addstate(l, s.out0)
        addstate(l, s.out1)
        return

    l.add s

proc startlist(start: var State, l: var List): List =
  inc listid
  addstate(l, start)
  return l



proc step(clist: var List, c: char, nlist: var List) =
  inc listid
  for s in clist:
    if s.kind == skValue and s.value == c:
      addstate(nlist, s.out0)


proc listcmp(l1, l2: List): int =
  if (l1.len < l2.len):
    return -1

  if (l1.len > l2.len):
    return 1

  for i in 0 ..< l1.len:
    if (l1[i] < l2[i]):
      return -1

    elif (l1[i] > l2[i]):
      return 1

  return 0

proc ptrcmp(a, b: ref): int =
  if (a < b):
    return -1

  elif (a > b):
    return 1

  else:
    return 0

var alldstates: seq[DState]
proc dstate(l: List): DState =
  sort(l[]) do (s1, s2: State) -> int:
    ptrcmp(s1, s2)

  var dp = 0
  var d: DState = alldstates[dp]
  echo dp, " -> "
  while not isNil(d):
    # FIXME I don't understand original C code good enough to rewrite it
    # into something same. Specifically, article says we are iterating over
    # binary tree here - that part is easy, but I literally can not find
    # any other place where `.left` and `.right` is referenced, which means
    # they are either not set at all (highly unlikely), or this is done via
    # some badshit-crazy C memory hacks, that I can'd decode.
    let i = listcmp(l, d.l)
    if i < 0:
      dp = d.left

    elif i > 0:
      dp = d.right

    else:
      echo "Found value"
      return d

    d = alldstates[dp]

  d = DState()
  new(d.l)

  alldstates[dp] = d
  return d



#   dp = &alldstates;
#   while ((d = *dp) != NULL):
#     let i = listcmp(l, &d->l);
#     if (i < 0):
#       dp = &d->left;

#     elif (i > 0):
#       dp = &d->right;

#     else:
#       return d

#   d = malloc(sizeof *d + l->n * sizeof l->s[0]);
#   memset(d, 0, sizeof *d);
#   d->l.s = (State**)(d + 1);
#   memmove(d->l.s, l->s, l->n * sizeof l->s[0]);
#   d->l.n = l->n;
#   *dp    = d;
#   return d;

proc startnfa(start: State, l: var List) =
  inc listid
  addstate(l, start);

proc startdstate(start: var State): DState =
  return dstate(startlist(start, l1))


proc nextstate(d: DState, c: char): DState =
  step(d.l, c, l1)
  d.next[c] = dstate(l1)
  return d.next[c]

proc match(start: DState, s: string): bool =
  var
    d: DState = start
    next: DState

  for c in s:
    next = d.next[c]
    if isNIl(next):
      next = nextstate(d, c)

    d = next

  return ismatch(d.l)

proc main() =
  var post  = re2post("a*b");
  echo post
  var start = post2nfa(post);
  echo isNil(start)
  new(l1); l1[] = newSeq[State](nstate)
  new(l2); l2[] = newSeq[State](nstate)
  alldstates = newSeq[DState](nstate)
  # l1.s         = malloc(nstate * sizeof l1.s[0]);
  # l2.s         = malloc(nstate * sizeof l2.s[0]);

  startHax()
  let str = "aaaab"
  echov match(startdstate(start), str)

main()
