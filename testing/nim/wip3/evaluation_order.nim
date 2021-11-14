import hmisc/core/all

proc main() =
  # template templ(arg: untyped): untyped = arg + arg

  block:
    var value = 0
    /// "Store values":
      /// "First":
        let a = (inc value; value)

      /// "Second":
        let b = (inc value; value)


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

  block:
    echo "----"
    var value = 0
    /// "Evaluate each expression as argument":
      echo cexpr("Evaluate first", (inc value; value)) +
        cexpr("Evaluate second", (inc value; value))

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
    var value = 0
    proc get(): int = inc value; return value

    /// "Call expression value":
      echo get() + get()

main()
