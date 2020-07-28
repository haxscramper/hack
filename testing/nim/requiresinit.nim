type
  Test = object
    a {.requiresinit.}: int
    b: int


echo Test(a: 12)
