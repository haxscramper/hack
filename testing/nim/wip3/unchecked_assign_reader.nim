import
  std/[
    strutils,
    tables,
    intsets,
    json,
    uri,
    options,
    typetraits
  ]

using
  buffer: var seq[string]
  pos: var int

proc store(buffer; val: int) = buffer.add $val
proc store(buffer; val: float) = buffer.add $val
proc store(buffer; val: string) = buffer.add val

proc load(buffer, pos; target: var int) =
  target = parseInt(buffer[pos])
  inc pos

proc load(buffer, pos; target: var string) =
  target = buffer[pos]
  inc pos

proc load(buffer, pos; target: var float64) =
  target = parseFloat(buffer[pos])
  inc pos


proc load[E: enum](buffer, pos; target: var E) =
  target = parseEnum[E](buffer[pos])
  inc pos

proc store[T: enum](buffer: var seq[string], target: T) =
  buffer.add $target

proc store[E](buffer; value: set[E]) =
  buffer.store(len(value))
  for item in items(value):
    buffer.store(item)

proc load[E](buffer, pos; value: var set[E]) =
  var count: int
  buffer.load(pos, count)
  for _ in 0 ..< count:
    var tmp: E
    buffer.load(pos, tmp)
    value.incl tmp

proc store[T](buffer; value: seq[T]) =
  buffer.store(len(value))
  for item in items(value):
    buffer.store(item)

proc load[E](buffer, pos; value: var seq[E]) =
  var count: int
  buffer.load(pos, count)
  for _ in 0 ..< count:
    var tmp: E
    buffer.load(pos, tmp)
    value.add tmp


proc load[T: object](buffer, pos; target: var T) =
  {.cast(uncheckedAssign).}:
    for name, field in fieldPairs(target):
      load(buffer, pos, field)


proc store[T: object](buffer: var seq[string], target: T) =
  for name, field in fieldPairs(target):
    buffer.store(field)


# proc load[T: distinct](buffer, pos; target: var T) =
#   type Base = distinctBase(T)
#   var base: Base = default(Base)
#   buffer.load(pos, base)
#   target = T(base)

# proc store[T: distinct](buffer; value: T) =
#   type Base = distinctBase(T)
#   buffer.store(Base(value))


type
  E = enum One, Two, Three
  W = ref object of RootObj
    a: int
  X = ref object of W
    b: int
  Y = object of RootObj
    a: int
  Z = object of Y
    b: int
  S = distinct string
  G = enum
    Even
    Odd
  F = object
    x: int
    y: float

  MyType = ref object
    a: int
    b: float
    c: string
    d: MyType
    e: G
    f: F
    g: (string, int)
    h: (VType, VType, VType, VType, VType)
    i: seq[string]
    j: Table[string, int]
    k: TableRef[string, int]
    l: IntSet
    m: JsonNode
    n: Uri
    o: seq[int]
    p: Option[F]
    q: array[G, string]
    # r: array[3, E]
    s: S
    t, u: int
    w: W
    x: X
    y: Y
    z: Z

  VType = object
    ignore: bool
    case kind: G
    of Even:
      even: int
    of Odd:
      odd: bool
      case also: uint8
      of 3:
        discard
      of 4:
        omg, wtf, bbq: float
      else:
        `!!!11! whee`: string

proc `==`(a, b: S): bool {.borrow.}

proc `==`(a, b: W | X): bool =
  if a.isNil == b.isNil:
    if a.isNil:
      true
    else:
      a[] == b[]
  else:
    false

proc `$`*(s: S): string = s.string

proc roundTrip[T](obj: T) =
  var buffer: seq[string]
  store[T](buffer, obj)
  var target: T
  var pos: int = 0
  load[T](buffer, pos, target)

  echo "---"
  echo target
  echo obj

roundTrip 46
# roundTrip Two
# roundTrip {Two, Three}
# roundTrip NimVersion
# roundTrip @["goats", "pigs"]
# roundTrip (food: "pigs", quantity: 43)
# roundTrip ("pigs", 43, 22.0, Three)
# roundTrip S"snakes"
roundTrip F(x: 4, y: 5.3)
# roundTrip W(a: 23)
# roundTrip X(a: 48, b: 59)
# roundTrip Y(a: 23)
# roundTrip Z(a: 23, b: 59)
# roundTrip @[{One, Two}, {Two, Three}]
# roundTrip [Three, One, Two]
# roundTrip ["three", "one", "two"]

# roundTrip %*{"a": 1}
# roundTrip %[1, 2, 3, 4, 4]
