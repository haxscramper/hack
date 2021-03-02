type
  SNested {.importcpp: "Nested", header: "enclass.hpp".} = enum
    valFixup0 = 0
    valFixup1 = 1
    val1 = 2
    val2 = 3

  RegularC {.importcpp: "RegularC".} = enum
    ecRC1
    ecRC2

  EnClass {.importcpp: "EnClass".} = enum
    ecEC1
    ecEC2

  RegularOffset {.importcpp: "RegularOffset".} = enum
    roRO1 = 1
    roRO2 = 2

let hello = {val1, val2} + {val1, valFixup0}
echo hello

echo {ecEC1} + {ecEC2}
echo {roRO1} + {roRO2}


# block:
#   var arr: array[RegularC, int]
#   echo arr

# block:
#   var arr: array[RegularOffset, int]
#   echo arr
