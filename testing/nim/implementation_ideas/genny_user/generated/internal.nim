proc genny_main_obj_get_data*(o: Obj): int {.raises: [], cdecl, exportc, dynlib.} =
  getData(o)

proc genny_main_obj_items_begin*(o: Obj): (iterator(): int) {.raises: [], cdecl, exportc, dynlib.} =
  return iterator(): int = 
    for item in items(o):
      yield item

proc genny_main_obj_items_get_next*(iter: ptr (iterator(): int)): int =
  return iter[]()

proc genny_main_obj_items_finished(iter: ptr (iterator(): int)): bool =
  return finished(iter[])
