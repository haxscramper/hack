import sequtils

let iter = iterator(): int =
             {.emit: "/* Closure iterator start */".}
             for i in 0 .. 10:
               {.emit: "/* closure iterator yield */".}
               yield i

for i in iter:
  echo i
