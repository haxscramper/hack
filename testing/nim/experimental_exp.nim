block:
  # Each 0 or 1 in (assignment?) will be replaced with `3`
  template t{0|1}(): untyped =
    static: echo "replaced with 3!"
    3

  block:
    let a = 1
    echo a

  block:
    let b = 0
    echo b

block:
  template echoSideefects{`*`(a, 2)}(a: int{call}): int =
    static: echo "Multiplying ", astToStr(a), " by two"
    block:
      let tmp = a
      tmp + tmp

  proc ff(): int =
    echo "aasd"
    8

  echo ff() * 2
  echo 1 * 2

block:
  proc expensiveParse(arg: string): int =
    echo "Running parse ..."
    90

  template parseOptimize{expensiveParse(pattern)}(pattern: string{lit}): int =
    static: echo "Using hoisting for call to `expensiveParse`"
    var gl {.global, gensym.} = expensiveParse(pattern)
    gl

  echo "Running loop"           # Printed after parse
  for i in 0..10:
    # The code for parsing will run at the start of the program: it
    # might save a lot of time for complicated parsing algorithms
    # (read whole configuration file)
    let t = expensiveParse("Literal string to parse")
    # Nothing will be printed in the loop - parsing code has already
    # been called.

    # But code WILL run of the argument is not a literal string.

  # IDEA `{.global, gensym.}` might be used for reading configuration
  # files at the start of the progam. You write function for parsing
  # user configuration, store all data in the struct and return it
  # each time the proc is called - no need to have explicit global
  # variable.
