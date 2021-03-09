# Expermined results: If I have `{.requiresinit.}` field in my type
# `mapIt` (or any template like that) fails to compile. I looked into
# the source code - compilation error appears because of `typeof((var
# it{.inject.}: typeof(items(s)); op))` - it is not possible to inject
# `{.requiresinit.}` var without initialization.

import sequtils
type
  Test = object
    a {.requiresinit.}: int

# Does not compile
# func test1*(args: varargs[Test]): seq[(int, Test)] =
#   args.mapIt((12, it))

# `mapIt` creates dummy variable in form `var it {.inject.}: T` to get
# type of the expression: it works for regular variables but
# completely fails for all other ones
# type OutType = typeof((
#      block:
#        var it{.inject.}: typeof(items(s), typeOfIter);
#        op), typeOfProc)

# typeof((var it{.inject.}: typeof(items(s)); op))

# echo typeof((var it: Test; it)) # Fails to compile
echo typeof((var tPtr: ref Test; let it = tPtr[]; it))



# Compiles
func test2*(args: varargs[Test]): seq[(int, Test)] =
  for it in args:
    result.add((12, it))
