import nimib


proc integralConstantValue(Tp: typedesc, V: Tp): Tp = V
proc integralConstantValueType(Tp: typedesc, V: Tp): Tp = discard

proc trueType(): auto = integralConstantValue(bool, true)
proc falseType(): auto = integralConstantValue(bool, false)

discard integralConstantValue(bool, true)

proc isIntegralHelper(T: typedesc): auto = falseType()
  ## cpp`template<typename> struct __is_integral_helper : public false_type { };`

proc isIntegralHelper(T: typedesc[bool]): auto = integralConstantValue(bool, true)
  ## cpp`template<> struct __is_integral_helper<bool> : public integral_constant<bool, true> { };`

proc isIntegralHelper(T: typedesc[cint]): auto = trueType()
  ## cpp`template<> struct __is_integral_helper<int> : public integral_constant<bool, true> { };`

proc removeConstType(T: typedesc): auto =
  if false:
    var tmp: T
    return tmp

proc removeVolatileType(T: typedesc): auto =
  ## C++ code has two 'implementations' for this - one with `volatile` is
  ## more specialized, so any type that is `volatile-qualified` will use
  ## this implementation and discard it afterwards.

  ## ```cpp
  ## template<typename _Tp>
  ##   struct remove_volatile
  ##   { typedef _Tp     type; };
  ##
  ## template<typename _Tp>
  ##   struct remove_volatile<_Tp volatile>
  ##   { typedef _Tp     type; };
  ## ```

  if false:
    var tmp: T
    return tmp


proc removeCvType(T: typedesc): auto =
  ##[

  ```cpp
     template<typename _Tp>
      struct remove_cv
      {
        typedef typename
        remove_const<typename remove_volatile<_Tp>::type>::type     type;
      };
  ```

  ]##
  if false:
    # cpp`typename remove_volatile<_Tp>::type`
    # Remove volatile type from `T`
    type tmp1 = typeof removeVolatileType(T)
    # cpp`typename remove_const< [tmp1] >::type`
    # Use volatile type
    type tmp2 = typeof removeConstType(tmp1)
    # 'return' resulting type
    var tmp3: tmp2
    return tmp3


proc isIntegralValue(T: typedesc): auto =
  ## ```cpp
  ## template<typename _Tp>
  ##   struct is_integral
  ##   : public integral_constant<bool, (__is_integral_helper<typename
  ## 			      remove_cv<_Tp>::type>::value)>
  ##   { };
  ## ```

  integralConstantValue(bool, isIntegralHelper(typeof removeCvType(T)))


proc isFloatingPointHelper(T: typedesc): auto =
  falseType()

proc isFloatingPointHelper(T: typedesc[float]): auto = trueType()

proc isFloatingPointValue(T: typedesc): auto =
  integralConstantValue(bool, isFloatingPointHelper(typeof removeCvType(T)))


{.emit: """/*INCLUDESECTION*/
#include <type_traits>
#include <iostream>
"""}

{.emit:"""
template <
  class T,
  typename std::enable_if<
    std::is_integral<T>::value, bool
  >::type* = nullptr
>
sfinaeOverload() {
  std::cout << "Calling integral type function\n";
}


template <
  class T,
  typename std::enable_if<
    std::is_floating_point<T>::value, bool
  >::type* = nullptr
>
sfinaeOverload() {
  std::cout << "Calling floating point type function\n";
}
""".}

proc sfinaeOverload[T]() {.noinit.} =
  proc initGenBaseImpl(Ttype: typedesc) {.importcpp: "sfinaeOverload<'*1>(@)".}
  static:
    assert isIntegralValue(T) or isFloatingPointValue(T)

  initGenBaseImpl(T)

static:
  echo isIntegralValue(bool)
  echo isIntegralValue(cshort)
  echo isIntegralValue(cint)
  echo isFloatingPointValue(float)


sfinaeOverload[cint]()
sfinaeOverload[float]()
