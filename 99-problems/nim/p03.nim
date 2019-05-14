#[
+ You have to explicitly write ~discard~ if you want ot ignore proc's
  return value
]#

proc k_th_element(data: seq[int], index: int = 0): int =
  data[index]

let data: seq[int] = @[2,4,5,3,2]

discard data.k_th_element(1)

echo k_th_element data
