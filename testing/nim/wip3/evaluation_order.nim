import hmisc/core/all

proc main() =
  # template templ(arg: untyped): untyped = arg + arg

  block:
    var value = 0
    /// "Store values":
      /// "First":
        inc value
        let a = value

      /// "Second":
        inc value
        let b = value


    echo a
    echo b

  block:
    echo "----"
    var value = 0
    /// "Echo both values":
      /// "First":
        echo (inc value; value)

      /// "Second":
        echo (inc value; value)

  cblock "No identity":
    echo "----- No identity"
    var value = 0
    func id(i: int): int = i
    echo (inc value; value) + (inc value; value)

  cblock "With mutator":
    echo "----- With mutator"
    var value = 0
    proc `?`(a, b: var int): int = a + b
    echo (inc value; value) ? (inc value; value)


  cblock "With identity":
    echo "----- With identity"
    var value = 0
    func id(i: int): int = i
    echo id((inc value; value)) + id((inc value; value))

  block:
    echo "----"
    var value = 0
    /// "Evaluate each expression as argument":
      echo cexpr("Evaluate first", (inc value; value)) +
        cexpr("Evaluate second", (inc value; value))

  cblock "With custom mutator":
    echo "----- with custom mutator"
    proc mut(arg: var int) = arg = arg + 1
    var value = 0
    echo (mut value; value) + (mut value; value)

  cblock "With custom mutator on strings":
    echo "----- with custom mutator on strings"
    proc mut(arg: var string) = arg = arg & "?"
    var value = ""
    echo (mut value; value) & (mut value; value)

  cblock "With object":
    echo "----- with object"
    type
      Object = object
        f1: int
        f2: array[2000, int]

    proc mut(arg: var Object) = arg.f1 = arg.f1 + 1
    var value: Object
    proc `+`(l, r: Object): int = l.f1 + r.f1
    echo (mut value; value) + (mut value; value)

  block:
    echo "----"
    proc `+++`(a, b: int): int =
      echo a
      echo b
      return a + b

    var value = 0
    /// "Pass expressions to user operator":
      echo cexpr("Evaluate first", (inc value; value)) +++
        cexpr("Evaluate second", (inc value; value))



  block:
    echo "----"
    proc get(): int {.nimcall.} =
      var value {.global.}: int
      inc value
      return value

    /// "Call expression value":
      echo get() + get()

main()
