proc generic[Seq: string | openarray](
  a: Seq,
  callback: proc(a: Seq)): void = callback(a)


echo string is openarray
proc openarr[T](a: openarray[T]): void = echo a
openarr("hello")

generic(
  "hello world",
  proc(a: string) = echo a
)

type
  Indexable[T] = concept t
    t[int] is T

echo string is Indexable[char]
echo seq[int] is Indexable[int]
echo array[int, int] is Indexable[int]

