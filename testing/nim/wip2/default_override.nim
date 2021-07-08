type
  CustomType = object
    field: string

proc default(T: typedesc[CustomType]): T =
  CustomType(field: "sfasdf")


var c = default(CustomType)
echo c
