#*************************************************************************#
#********************  Base definition of iterators  *********************#
#*************************************************************************#

type
  InputIteratorImpl[T] {.importcpp: "'0::iterator", nodecl.} = object
  ForwardIteratorImpl[T] {.importcpp, nodecl.} = object

  InputIterator[T] = InputIteratorImpl[T] | ForwardIteratorImpl[T]

proc inc[T](it: var InputIterator[T]): void {.importcpp: "(++#)".}
  ## Increment iterator position


proc `==`[T](it1, it2: InputIterator[T]): bool {.importcpp: "(# == #)".}
  ## Equality comparison

iterator cxItems[T](obj: T): auto =
  mixin cxBegin, cxEnd
  var start = obj.cxBegin()
  var finish = obj.cxEnd()
  while start != finish:
    yield start[]
    inc start

#*************************************************************************#
#********************  Unordered set implementation  *********************#
#*************************************************************************#

type
  StdUnorderedSet[T] {.
    importcpp: "std::unordered_set",
    header: "<unordered_set>"
    .} = object

proc initStdUnorderedSet[T](): StdUnorderedSet[T]
  {.importcpp: "std::unordered_set<'0>()", constructor.}

#===========  Implementation-specific overloads for iterators  ===========#

proc cxBegin[T](sset: StdUnorderedSet[T]): InputIteratorImpl[StdUnorderedSet[T]]
  {.importcpp: "(#.begin())", header: "<unordered_set>".}

proc cxEnd[T](sset: StdUnorderedSet[T]): InputIteratorImpl[StdUnorderedSet[T]]
  {.importcpp: "(#.end())", header: "<unordered_set>".}

proc insert[T](sset: var StdUnorderedSet[T], item: T): void
  {.importcpp: "(#.insert(@))".}

proc `[]`[T](it: InputIterator[StdUnorderedSet[T]]): T {.importcpp: "(*#)"}
  ## Dereference operator with correct mapping for unordered iterators


iterator items[T](sset: StdUnorderedSet[T]): T =
  for item in cxItems(sset):
    yield item

#*************************************************************************#
#******************************  Std tuple  ******************************#
#*************************************************************************#

type
  StdTuple2[T0, T1] {.importcpp: "std::tuple", header: "<tuple>".} = object


  StdTuple = StdTuple2


proc get0[T0, T1](cxTuple: StdTuple2[T0, T1]): T0 {.importcpp: "std::get<0>(@)".}
proc get1[T0, T1](cxTuple: StdTuple2[T0, T1]): T1 {.importcpp: "std::get<1>(@)".}

proc get(cxTuple: StdTuple, idx: static[int]): auto =
  when idx == 0:
    return get0(cxTuple)

  elif idx == 1:
    return get1(cxTuple)

#*************************************************************************#
#****************************  Unordered map  ****************************#
#*************************************************************************#

type
  StdUnorderedMap[K, V] {.
    importcpp: "std::unordered_map",
    header: "<unordered_map>"
    .} = object

proc initStdUnorderedMap[K, V](): StdUnorderedMap[K, V]
  {.importcpp: "std::unordered_map<'0, '1>()", constructor.}

#=============  Implementation-specific iterator overrides  ==============#
proc cxBegin[K, V](sset: StdUnorderedMap[K, V]): InputIteratorImpl[StdUnorderedMap[K, V]]
  {.importcpp: "(#.begin())", header: "<unordered_set>".}

proc cxEnd[K, V](sset: StdUnorderedMap[K, V]): InputIteratorImpl[StdUnorderedMap[K, V]]
  {.importcpp: "(#.end())", header: "<unordered_set>".}

proc `[]=`[K, V](sset: var StdUnorderedMap[K, V], key: K, val: V): void
  {.importcpp: "(#[#] = #)".}

proc `[]`[K, V](it: InputIterator[StdUnorderedMap[K, V]]): StdTuple2[K, V] {.importcpp: "(*#)"}
  ## Dereference operator with correct mapping for unordered iterators

iterator items[K, V](smap: StdUnorderedMap[K, V]): StdTuple2[K, V] =
  for val in cxItems(smap):
    yield val

iterator pairs[K, V](smap: StdUnorderedMap[K, V]): (K, V) =
  for val in cxItems(smap):
    yield (val.get(0), val.get(1))

proc main() =
  var sset = initStdUnorderedSet[cint]()
  sset.insert cint(12)
  sset.insert cint(33)
  for it in items(sset):
    echo it

  var smap = initStdUnorderedMap[cint, cint]()
  smap[cint(12)] = cint(2)
  smap[cint(90)] = cint(10)

  for key, val in pairs(smap):
    echo key, " -> ", val

main()
