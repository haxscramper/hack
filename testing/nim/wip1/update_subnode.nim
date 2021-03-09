import macros

macro test(): untyped =
  var tmp = newLit("hello")

  result = quote do:
    echo `tmp` & "nice"

  echo result.toStrLit()

  tmp.strVal = "Next gen"

  echo result.toStrLit()


test()
