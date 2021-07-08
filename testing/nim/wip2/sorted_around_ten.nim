import std/[heapqueue]

var heap = initHeapQueue[int]()
for v in [50, 1, 6, 3, 10, 22, 32, 33, 34, 40]:
  heap.push(v)

echo heap
