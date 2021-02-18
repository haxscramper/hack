import hnimast

proc printTest*(arg: cstring): cstring {.exportc.} =
  echo "Calling nim functions"
  echo "arg: ", arg

  let node = parsePNodeStr($arg)

  let res = treeRepr(node, colored = false)
  result = res.cstring
# result = arg

  echo "returning from nim function"
  echo "-----"
