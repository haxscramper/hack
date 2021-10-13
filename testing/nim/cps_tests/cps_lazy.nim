
import cps, std/[deques, macros, sugar]

type
  Stream[T] = ref object of Continuation
    val: T
    sIn: Stream[T]

proc jield(s: Stream, val: int = 0): Stream {.cpsMagic.} =
  s.val = val

proc getSin(s: Stream): Stream {.cpsVoodoo.} =
  s.sIn

proc resume[T](s: Stream[T]): T =
  block:
    var s = Continuation: s
    while s.running:
      s = s.fn(s)
  result = s.val

macro stream(n: untyped): untyped =
  n.addPragma nnkExprColonExpr.newTree(ident"cps", ident"Stream")
  n

template `->`[T](ca: Stream[T], b: typed): Stream[T] =
  let cb = whelp(b)
  cb.sIn = ca
  cb

template `->`[T](a, b: typed): Stream[T] =
  let ca = whelp(a)
  let cb = whelp(b)
  cb.sIn = ca
  cb


###########################################################################

proc toStream[T](r: Hslice[T, T]) {.stream.} =
  var i = r.a
  while i <= r.b:
    jield(i)
    inc i

proc map[T](fn: proc(x: T): T) {.stream.} =
  let sIn = getSin()
  while true:
    let v = fn(sIn.resume())
    if not sIn.running: break
    jield(v)

proc filter[T](fn: proc(x: T): bool) {.stream.} =
  let sIn = getSin()
  while true:
    let v = sIn.resume()
    if not sIn.running: break
    if fn(v):
      jield(v)

proc print() {.stream.} =
  let sIn = getSin()
  while true:
    let v = sIn.resume()
    if not sIn.running: break
    echo v
    jield()

proc pump() {.stream.} =
  let sIn = getSin()
  while sIn.running:
    discard sIn.resume()

var s = Continuation:
  toStream(1..10) ->
  map(x => x * 3) ->
  filter(x => (x mod 2) == 0) ->
  print() ->
  pump()

while s.running:
  s = s.fn(s)
