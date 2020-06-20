let file = "final.nim".open(fmWrite)
file.writeLine """
proc farg(fa: proc(a: int) {.nimcall.}) = fa(12)

proc matcher(arg: int, pr: proc(arg: int): string {.nimcall.}): string =
  pr(arg)
"""

for i in 0 .. 1000000:
  file.writeLine "echo compiles(matcher(0, toPStr))"

file.close()
