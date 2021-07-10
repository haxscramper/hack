{.emit: """/*TYPESECTION*/
struct Base {
  int baseField;
  int baseMethod() { return 12; }
  virtual int toOverride() { return 42; }

};



struct Derived : public Base {
  int derivedField;
  int toOverride() override { return 99; }
};

struct Other {};
""".}

type
  CxxBase {.importcpp: "Base", byref, inheritable.} = object
    baseField: cint

  CxxDerived {.importcpp: "Derived", byref.} = object of CxxBase
    derivedField: cint

  CxxDerived2 = object of CxxBase
    derivedField: cint

  CxxOther {.importcpp: "Other", byref.} = object


proc aux() {.header: "<new>", importc: "//".}

method toOverride(base: ref CxxBase): cint {.base.} =
  {.emit: "return `base`->toOverride();".}

method toOverride(base: CxxBase): cint {.base.} =
  {.emit: "return `base`->toOverride();".}


proc newCxxBase(): ref CxxBase  =
  aux()
  new(result)
  {.emit: "new ((void*)result) Base();".}

proc newCxxDerived(): ref CxxDerived  =
  aux()
  new(result)
  {.emit: "new ((void*)result) Derived();".}
  
proc newCxxDerived2(): ref CxxDerived2  =
  new(result)

proc newCxxOther(): ref CxxOther =
  aux()
  new(result)
  {.emit: "new ((void*)result) Other();".}

echo CxxBase().toOverride()
echo CxxDerived().toOverride()

echo newCxxBase().toOverride()
echo newCxxDerived().toOverride()

method toOverride(derived: CxxDerived2): cint =
  echo "Overide method impementation from nim side"
  return 1231.cint

echo newCxxDerived2().toOverride()
# echo newCxxOther().toOverride()
