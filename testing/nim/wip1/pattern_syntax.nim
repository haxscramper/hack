import macros

dumpTree:
  {
    # Match pattern, otherwise execute `doError`
    "key": @a is ("2" | "3") isnot doError(),
    # execute regular code
    "key": @a is ("2" | "3") isnot (
      echo "Expected `2` or `3`"
    ),
    # Do nothing on fail
    "key": @a is ("2" | "3"),
    # Execute callback if match failed
    "key": "2" | "3" isnot doError(),
    # Check for match
    "key": "2" | "3",

    "key": _.isString() isnot (echo "Expected string for key"),
    "key": @a.isInt() isnot (echo "expected integer")
  }

  {"menu" : {"file": @file is JString() isnot (
    echo "Expected string but got", it)}}

  (
    fld: ForStmt([
      # First element must be an `nnkIdent("..")`
      == ident("..") isnot (
        echo "Expected infix `..` on ", it.lineInfoObj()
      ),

      # Second element should have kind `IntLit()` and will be bound
      # to injected vaeriable `a`
      @a is IntLit() isnot (
        echo "Expected integer literal `..`", it.lineInfoObj()
      ),

      @a is (
        IntLit() or StrLit()
      ) isnot (
        echo "Expected either integer literal or string but found " &
          $it.lineInfoObj()
      )
    ])
  )

  any _(if it != head: tail.add it)
  fld: @a(it mod 2 == 0)
  [any @a, until 6]

  {
    "key" isnot (echo "expected to have `key`"): (
      JString() isnot (echo "Expected string value for key")
    )
  }
  # [any @a until 6]

  case 1:
    of _.startsWith("hello"): discard

import macros
macro toBool(arg: typed): untyped =
  result = getType(arg)
  # echo result.lispRepr()
  if (result.kind == nnkSym) and (result.strVal() in ["void", "bool"]):
    if result.strVal() == "bool":
      return arg
    else:
      return quote do:
        block:
          `arg`
          true
  elif result.kind == nnkBracketExpr and
       result[0].strVal() in ["ref", "ptr"]:
    return quote do:
      block:
        `arg` != nil


  error("Expected either `void` or `bool` expression, but found " &
    result.toStrLit().strVal(), arg)

echo toBool(echo 12)
echo toBool(true)
var test: ref int
echo toBool(test)
echo toBool(9 + 2)
