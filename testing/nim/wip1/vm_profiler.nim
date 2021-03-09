import tables

static:
  var table: Table[string, string]
  for i in 0 .. 50:
    table[$i & "eeeee"] = $i & "e===="
