# TODO constant and regular iterator support
type
  Iterator[Type, State] = object
    state: State
    nextImpl*: proc(s: var State): Type
    prevImpl*: proc(s: var State): Type
    atImpl*: proc(s: var State, index: int): Type
    atEndImpl*: proc(s: var State): bool

  ForwardIter[Type, State] = object
    base: Iterator[Type, State]

  BidirIter[Type, State] = object
    base: Iterator[Type, State]

iterator items[T, S](iter: var BidirIter[T, S]): T =
  assert iter.base.nextImpl != nil, "Missing `nextImpl` for iterator"
  assert iter.base.atEndImpl != nil, "Missing `atEndImpl` for iterator"

  while not iter.base.atEndImpl(iter.base.state):
    yield iter.base.nextImpl(iter.base.state)

proc begin[T](s: ref seq[T]): BidirIter[T, tuple[idx: int, s: ref seq[T]]] =
  type State = tuple[idx: int, s: ref seq[T]]

  result.base.state = (0, s)

  result.base.atEndImpl =
    proc(state: var State): bool =
      state.idx >= state.s[].len

  result.base.nextImpl =
    proc(state: var State): T =
      result = state.s[state.idx]
      inc state.idx

proc toRef[T](s: seq[T]): ref seq[T] =
  new(result)
  for it in s:
    result[].add it

var iter = @[1,2,3].toRef().begin()
for it in iter:
  echo it
