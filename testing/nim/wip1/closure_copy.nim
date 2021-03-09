import std/[sugar]

proc main() =
  block:
    var arr: array[10, proc()]
    for item in mitems(arr):
      item = (proc() = echo 12)

  block:
    var arr: array[10, proc(arg: int): string]
    for item in mitems(arr):
      item = (proc(arg: int): string = $arg)


  block:
    var arr: array[10, proc(arg: int): string]
    let env = 12
    for item in mitems(arr):
      item = (proc(arg: int): string = $(env + arg))

  block:
    var arr: array[10, proc(arg: int): string]
    var env = 12
    for item in mitems(arr):
      item = (proc(arg: int): string = $(env + arg))


main()
