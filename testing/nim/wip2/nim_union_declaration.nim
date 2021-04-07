type
  Union {.union.} = object
    field1: int
    field2: float

  Struct = object
    field1: int
    field2: float


echo Union()
echo Struct()
