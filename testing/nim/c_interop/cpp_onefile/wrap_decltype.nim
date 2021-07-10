{.passc:"-std=c++20".}
{.emit:"""/*INCLUDESECTION*/
#include <type_traits>
""".}

when true:
  {.emit: """
// Decltype declaration
template <typename T>
auto test1(T arg, decltype(arg + arg) arg2) -> decltype(arg + arg2) {
  return arg + arg2;
}

  """.}

  proc test1[T0, T1](arg: T0, arg2: T1): auto =
    when arg2 isnot typeof(arg + arg):
      {.error: "C++ wrapper type constraint fail".}

    else:
      if false:
        # The code is never run, and only needed for correct initialization
        # of the procedure return type.
        var tmp: ptr typeof(arg + arg)
        result = tmp[]

      {.emit: "return test1(`arg`, `arg2`);".}

  echo test1(1, 2)
  echo not compiles test1(1, 0.2)


when false:
  {.emit: """
// auto return with constexpr if
template <typename T>
constexpr auto test2(T arg) {
  if constexpr (std::is_same<T, int>) {
    return "test";
  } else {
    return arg;
  }
}
  """.}


  proc test2[T](arg: T): auto =
    return false

  echo test2(1)
  echo test2(0.3)

when true:
  {.emit: """
// Sfinae constrain
template <typename T, std::enable_if_t<std::is_integral<T>::value, bool> = true>
T test3(T arg) { return arg; }
""".}

  proc test3[T](arg: T): T {.importcpp: "test3(@) /* call test3 */".} =
    static:
      assert arg is SomeInteger,
       "C++ SFINAE assert failure `std::enable_if_t<std::is_integral<T>::value`"

  proc test3Raw[T, Ret](arg: T): Ret
    {.importcpp: "test3(@) /* call raw test3 */".}

  echo test3(12)
  echo test3Raw[int, int](12)
