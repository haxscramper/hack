{.emit: """/*TYPESECTION*/
class TestExport {
  public:
    int field;
    TestExport() : field(20) { }
};
""".}

type
  TestExport {.importcpp: "TestExport".} = object
    field: cint

proc TestExportConstr(): TestExport {.constructor, importcpp: "TestExport()".}

proc newTestExport*(): TestExport {.
  exportc: "newTestExport", dynlib, noInit.} =
  TestExportConstr()

echo newTestExport()
