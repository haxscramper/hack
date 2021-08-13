import std/[strutils, macros]

type
  VariantKind = enum vkFirst, vkSecond
  VariantKind2 = enum vk2First, vk2Second

  Variant = object
    case kind: VariantKind
      of vkFirst:
        first: int

      of vkSecond:
        second: string

    case kind2: VariantKind2
      of vk2First:
        first2: int

      of vk2Second:
        second2: string


macro expandTree(arg: typed): untyped =
  echo arg.treeRepr()
  result = arg

template setKind*[T, K](t: var T, kind: untyped, target: K) =
  cast[ptr K](cast[int](addr t) + offsetOf(typeof(t), kind))[] = target


proc load(buffer: seq[string], pos: var int, target: var int) =
  target = parseInt(buffer[pos])
  inc pos

proc load(buffer: seq[string], pos: var int, target: var string) =
  target = buffer[pos]
  inc pos

proc load[E: enum](buffer: seq[string], pos: var int, target: var E) =
  target = parseEnum[E](buffer[pos])
  inc pos

proc load(buffer: seq[string], pos: var int, target: var Variant) =
  expandMacros:
    for name, field in fieldPairs(target):
      # This part requires codegen base on `typed` macros
      when name == "kind":
        block:
          var kind: VariantKind
          load(buffer, pos, kind)
          # C-specific hack, would not work on any other backend
          setKind(target, kind, kind)

      elif name == "kind2":
        block:
          var kind: VariantKind2
          load(buffer, pos, kind)
          setKind(target, kind2, kind)

      else:
        load(buffer, pos, field)

proc write[T](buffer: var seq[string], target: T) =
  buffer.add $target


proc write(buffer: var seq[string], target: Variant) =
  for name, field in fieldPairs(target):
    buffer.write(field)

var buffer: seq[string]
let objFrom = Variant(kind: vkSecond, second: "string")
buffer.write(objFrom)
var target: Variant
var pos: int = 0
buffer.load(pos, target)
echo objFrom
echo target

echo buffer

expandTree:
  echo target.kind
