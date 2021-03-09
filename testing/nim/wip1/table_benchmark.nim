import tables, times, strformat

for size in @[2, 4, 8, 32, 64, 256, 1024, 2048, 4096]:
  var table = initTable[int, int](size)
  let start = cpuTime()
  for i in 0 .. 1000:
    table[1] = 2
    table[0] = 2
    table[0] = table[1]

  echo fmt("Time for {size:<4}: {cpuTime() - start}")
