import strformat

proc definedLocally[T](u: T) = discard

iterator items[T](u: T): T = discard
proc gen*[T](u: T): T =
  mixin items # Has no effect
  definedLocally(u)

  # Fails to compile with type mismatch on `items`
  # for val in u: discard

  # If `items` is used explicitly everyting works as expected
  for val in items(u): discard
