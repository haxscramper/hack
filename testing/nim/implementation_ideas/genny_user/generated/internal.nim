proc genny_main_obj_get_data*(o: Obj): int {.raises: [], cdecl, exportc, dynlib.} =
  getData(o)

