proc farg(fa: proc(a: int) {.nimcall.}) = fa(12)

proc matcher(arg: int, pr: proc(arg: int): string {.nimcall.}): string =
  pr(arg)
