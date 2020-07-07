proc impl(a, b: int): void =
  echo "integer implementation"

proc generic[T](a, b: T): void =
  mixin impl
  when compiles(impl(a, b)):
    impl(a, b)
  else:
    echo "Generic fallback"

# Using implementation defined *before* `generic`
generic(12, 2)

proc impl(a, b: string): void =
  echo "string implementation"

# Using implementation defined *after* `generic`
generic("12", "123")

proc wrapper(): void =
  # Using implementation defined in the same scope as `generic` ???
  proc impl(a, b: float): void {.inject.} =
    echo "float implementation"

  # impl(1.2, 3.4)
  generic(1.2, 2.3)

wrapper()

#===============  pass implementation callbacks directly  ================#
proc genericCallback[T, Cb](a: T, cb: Cb): void =
  var called: bool = false
  # Iterate over all callbacks
  for name, fld in fieldPairs(cb):
    static:
      assert (fld is proc),
        "Expected proc for callback"

    # If can use callback, do it
    when compiles(fld(a)):
      if not called: # In case multiple callbacks work, use first one
        fld(a)
      called = true

genericCallback(
  "123412",
  (
    (proc(a: float): void = echo "Float implementation"),
    (proc(b: string): void = echo "String implementation"),
    (proc(b: string): void = echo "Second string callback")
  )
)
