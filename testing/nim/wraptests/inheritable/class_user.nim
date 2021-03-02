type
  Class {.importcpp, inheritable, pure, header: "class.hpp".} = object
    fld1*: cint

  UserType = object

  Class3 {.nodecl, importc.} = object of Class
    fld2*: UserType

{.emit: """/*TYPESECTION*/
// Manual declaration of derived class
struct `Class3` : Class {
  `UserType` fld2;
  `Class3`(int arg) : Class(arg) {}
};
""".}

proc initClass3*(arg: int): Class3 {.importcpp: "Class3(@)", constructor.}
proc cxxMethod*(self: Class): void {.importcpp: "#.cxxMethod(@)".}

proc main() =
  let cl3 = initClass3(24)
  cl3.cxxMethod()

  echo cl3.fld1

main()
