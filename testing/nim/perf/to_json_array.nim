import pkg/[jsony, benchy], std/[json]

timeIt "Json to array":
  let j = $(%[1,2,3])

timeIt "Jsony to array":
  let j = toJson([1,2,3])
