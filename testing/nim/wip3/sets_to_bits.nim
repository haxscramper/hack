import std/bitops

proc setcast[I: uint8 | uint16 | uint32 | uint64 | cint; E](s: set[E]): I =
  static:
    assert sizeof(s) <= sizeof(I),
     "Set cast integer size mismatch - sizeof(" & $I & ") was " & $sizeof(I) &
       ", while size of the " & $set[E] & " is " & $sizeof(s) &
       ". Correct target type for the set would be " & (
         case sizeof(s):
           of 1: "uint8 or more"
           of 2: "uint16 or more"
           of 3: "uint32 or more"
           of 4: "uint64 or more"
           else: "byte array array[" & $sizeof(s) & ", uint8]"
       )

  return cast[I](s)


proc getBits[I](i: I): seq[int] =
  for bit in countdown(sizeof(i) * 8 - 1, 0):
    result.add int(testBit(i, bit))

type En1 = enum en1Item
assert getBits(setcast[uint8, En1]({en1Item})) == @[0, 0, 0, 0, 0, 0, 0, 1]

type En2 = enum en2Item1, en2Item2
assert getBits(setcast[uint8, En2]({en2Item1, en2Item2})) == @[0, 0, 0, 0, 0, 0, 1, 1]

type
  En9 = enum
    en9Item1
    en9Item2
    en9Item3
    en9Item4
    en9Item5
    en9Item6
    en9Item7
    en9Item8
    en9Item9

assert getBits(setcast[uint16, En9]({en9Item1})) == @[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
assert setcast[uint16, En9]({en9Item1}) == 1
assert setcast[uint16, En9]({en9Item2}) == 2
assert setcast[uint16, En9]({en9Item2, en9Item1}) == 3

assert getBits(setcast[uint16, En9]({en9Item1, en9Item5, en9Item9})) == @[
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1]

type
  CE = enum
    ce1 = 1 shl 0
    ce2 = 1 shl 2

  NE = enum
    ne1
    ne2

echo cast[cint]({ne1, ne2})
echo ce2.ord or ce1.ord
