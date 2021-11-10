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
proc store(buffer; val: BiggestInt) = buffer.add $val
proc store(buffer; val: bool) = buffer.add $val
proc store(buffer; val: float) = buffer.add $val
proc store(buffer; val: string) = buffer.add val

proc load(buffer, pos; target: var bool) =
  target = parseBool(buffer[pos])
  inc pos

proc load(buffer, pos; target: var int) =
  target = parseInt(buffer[pos])
  inc pos

proc load(buffer, pos; target: var BiggestInt) =
  target = parseBiggestInt(buffer[pos])
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


proc store[R, V](buffer; value: array[R, V]) =
  buffer.store(len(value))
  for item in items(value):
    buffer.store(item)

proc load[R, V](buffer, pos; value: var array[R, V]) =
  var count: int
  buffer.load(pos, count)
  for idx in low(value) .. high(value):
    var tmp: V
    buffer.load(pos, tmp)
    value[idx] = tmp

proc store[K, V](buffer; value: OrderedTable[K, V]) =
  buffer.store(len(value))
  for key, value in pairs(value):
    buffer.store(key)
    buffer.store(value)

proc load[K, V](buffer, pos; target: var OrderedTable[K, V]) =
  var count: int
  buffer.load(pos, count)
  for _ in 0 ..< count:
    var key: K
    buffer.load(pos, key)

    var value: V
    buffer.load(pos, value)

    target[key] = value


type Object = object | tuple | ref object

proc load[T: Object](buffer, pos; target: var T) =
  when target is ref:
    if buffer[pos] == "nil":
      target = nil
      inc pos

    else:
      assert buffer[pos] == "ok"
      inc pos
      # target = default(T)
      new(target)
      {.cast(uncheckedAssign).}:
        for name, field in fieldPairs(target[]):
          load(buffer, pos, field)

  else:
    {.cast(uncheckedAssign).}:
      for name, field in fieldPairs(target):
        load(buffer, pos, field)


proc store[T: Object](buffer: var seq[string], target: T) =
  when target is ref:
    if isNil(target):
      buffer.add "nil"

    else:
      buffer.add "ok"
      for name, field in fieldPairs(target[]):
        buffer.store(field)

  else:
    for name, field in fieldPairs(target):
      buffer.store(field)


proc load[T: distinct](buffer, pos; target: var T) =
  type Base = distinctBase(T)
  var base: Base = default(Base)
  buffer.load(pos, base)
  target = T(base)

proc store[T: distinct](buffer; value: T) =
  type Base = distinctBase(T)
  buffer.store(Base(value))


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
proc `$`*(w: W): string = $(w[])

proc roundTrip[T](obj: T) =
  var buffer: seq[string]
  store(buffer, obj)
  echo "---"
  echo buffer
  echo typeof obj

  var target: T
  var pos: int = 0
  load(buffer, pos, target)

  echo target
  echo obj

roundTrip 46
roundTrip Two
roundTrip {Two, Three}
roundTrip NimVersion
roundTrip @["goats", "pigs"]
roundTrip (food: "pigs", quantity: 43)
# roundTrip ("pigs", 43, 22.0, Three)
roundTrip S"snakes"
# roundTrip F(x: 4, y: 5.3)
roundTrip W(a: 23)
# roundTrip X(a: 48, b: 59)
# roundTrip Y(a: 23)

roundTrip Z(a: 23, b: 59)
roundTrip @[{One, Two}, {Two, Three}]
roundTrip [Three, One, Two]
roundTrip ["three", "one", "two"]

roundTrip %*{"a": 1}
roundTrip %[1, 2, 3, 4, 4]
