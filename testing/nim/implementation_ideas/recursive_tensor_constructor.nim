import std/strformat

type
  Tensor[T] = ref object
    shape: seq[int]
    container: seq[T]

proc `[]`[T](tensor: Tensor[T], i: varargs[int]): int =
  var idx = 0
  for dimension in 1 ..< len(i):
    idx = i[dimension - 1] * tensor.shape[dimension] + i[dimension]

  return tensor.container[idx]

proc stripGeneric[T](s: typedesc[seq[T]]): auto =
  when T is seq:
    var tmp: typeof stripGeneric(T)
    return tmp
  else:
    var tmp: T
    return tmp

proc stripGeneric[T](s: seq[T]): auto =
  var tmp: typeof stripGeneric(typeof s)
  return tmp

proc newTensor*[T: not seq](input: seq[T]): Tensor[T] =
    new(result)
    result.container = input
    result.shape = @[input.len]

proc newTensor[T: seq](elements: seq[T]): auto =
  var res: Tensor[typeof stripGeneric(elements)]
  new(res)
  res.shape = @[len(elements)]
  var lastShape: seq[int]
  for sub in items(elements):
    let t = newTensor(sub)
    res.container &= t.container
    lastShape = t.shape

  res.shape &= lastShape

  return res

proc newTensor*[T: not seq](shape: seq[int], input: seq[T]): Tensor[T] =
    new(result)
    assert len(shape) == 1, &"invalid shape {shape} for 1D tensor"
    assert len(input) == shape[0], &"invalid input for tensor of shape {shape}"
    result.container = input
    result.shape = shape

proc newTensor[T: seq](shape: seq[int], elements: seq[T]): auto =
  var res: Tensor[typeof stripGeneric(elements)]
  new(res)
  res.shape = shape
  assert shape[0] == len(elements)
  for sub in items(elements):
    # FIXME inefficient as it involves copying for all subtensors. More
    # optimized would probably involve passing `var container: seq[T]`
    # buffer (from toplevel tensor) and appending elements to it (using
    # additional heuristics to guess optimal initial size would also speed
    # things up as you won't have to reallocate)
    res.container &= newTensor(shape[1..^1], sub).container

  return res

echo newTensor(@[1], @[1])[]
echo newTensor(@[1, 1], @[@[1]])[]
echo newTensor(@[1, 1, 1], @[@[@[1]]])[]
echo newTensor(
  @[@[@[@[@[@[@[@[
    @[@[@[@[@[@[@[1]]]]]]],
    @[@[@[@[@[@[@[1]]]]]]],
    @[@[@[@[@[@[@[1]]]]]]],
  ]]]]]]]])[]
