# TODO is it possible to return `typedesc` as value from proc? Is it
# possible to treat types as /values/?

func test(a: typedesc): typedesc = (if a is float: a else: string)
template ttest(a: typedesc): untyped = (when a is float: a else: string)

type
  Generic[T] = object
    f: T

var val: Generic[test(string)]
#                ^^^^^^^^^^^^^
#                |
#                Expands to void
# val.f = 1.2
# ^^^^^
# |
# Error: attempting to call undeclared routine: 'f='

var val2: Generic[ttest(int)]
val2.f = "123"
