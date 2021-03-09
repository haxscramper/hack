{.experimental: "views".}

func returnValue(): seq[string] =
  @["hello", "wolrd"]

block:
  let vals: lent seq[string] = returnValue()
  if false:
    echo vals # Segmentation fault

block:
  when false:
    # C codegen error
    let vals: openarray[string] = returnValue()

block:
  var elems: seq[lent string]
  let invals: seq[string] = @["Hello", "world"]
  for val in invals:
    elems.add val

  echo elems
