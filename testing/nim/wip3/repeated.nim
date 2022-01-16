proc identity[T](a: T): auto =
  echo "    inside of the identity", a
  a

proc idmsg[T](a: T, msg: string): T =
  echo msg
  return a

iterator filter*[T](s: openArray[T], pred: proc(x: T): bool {.closure.}): T {.effectsOf: pred.} =
  # for i in 0 ..< s.len
  var i = 0
  echo "  before while loop"
  while i < len(s):
    echo "  calling predicate check"
    if pred(idmsg(s[i], "    reading for pred")):
      yield idmsg(s[i], "    reading for yield")
    inc(i)

  echo "after while loop"

echo "iterating items"
for x in @[1,2,3,4].identity().items():
  discard
echo "done"

echo "iterating filter"
for x in @[5,6,7,8].identity().filter(proc (x: int): bool = x < 3):
  discard
echo "done"
