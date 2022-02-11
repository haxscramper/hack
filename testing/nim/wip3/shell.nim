import std/lenientops

type
  Handle = object
    id: uint16

  Counter = object
    value: uint16

const
  counterShift = 5
  counterMask = 0b1110_0000
  idMask =      0b0001_1111

func toHandle(idx: uint16, counter: Counter): Handle =
  result.id = idx or ((counter.value and counterMask) shl counterShift)

func `$`(handle: Handle): string =
  result.add $((handle.id and counterMask) shr counterShift)
  result.add "."
  result.add $(handle.id and idMask)

type
  ObjectStore[T] = object
    counts: seq[Counter]
    data: seq[T]
    empty: set[uint16]

func pop[I](s: var set[I]): I =
  for item in items(s):
    result = item
    s.excl item
    return

func initStore[T](): Objectstore[T] = discard

func forIndex(counts: var seq[Counter], idx: uint16): Counter =
  if counts.high < int(idx):
    counts.add Counter()

  counts[idx].value = (counts[idx].value + 1) and counterMask
  return counts[idx]

func initNew[T](store: var ObjectStore[T]): Handle =
  var idx =
    if len(store.empty) == 0:
      uint16(store.data.len())

    else:
      store.empty.pop()

  if store.data.high < int(idx):
    store.data.add default(T)

  else:
    store.data[idx] = default(T)

  return toHandle(idx, store.counts.forIndex(idx))

type
  Obj = object
    field1: int
    field2: float

proc main() =
  var store = initStore[Obj]()
  var h1 = initNew(store)
  var h2 = initNew(store)

  echo h1
  echo h2

main()
