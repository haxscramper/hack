type
  CharTraits[CharT] {.
    header: "<string>", importcpp: "std::char_traits".} = object

  AllocatorRebind[T] {.importcpp: "std::allocator<'0>::rebind"} = object

  Allocator[T] {.
    header: "memory", importcpp: "std::allocator".} = object

    rebind: typedesc[AllocatorRebind[T]]

  StdSizeT = cint

  StdAllocTraitsRebind[Alloc, Tp] {.
    importcpp: "__gnu_cxx::__alloc_traits<'0>::template rebind<'1>"
  .} = object

    other: typedesc[typeof(Alloc.rebind)]

  StdAllocTraits[Alloc] {.importcpp: "std::alloc_traits".} = object
    sizeType: StdSizeT
    # rebind: 

  ExtAllocTraits[Alloc] {.header: "<memory>".} = object
    baseType: typedesc[StdAllocTraits[Alloc]]
    sizeType: typedesc[typeof(baseType.sizeType)]

  BasicString[CharT, Traits, Alloc] {.
    header: "<string>", importcpp: "std::basic_string".} = object

    traitType:     typedesc[typeof(Traits)]
    charAllocType: typedesc[typeof(ExtAllocTraits[Alloc].other)]
    allocTraits:   typedesc[typeof(ExtAllocTraits[charAllocType])]
    sizeType:      typedesc[typeof(allocTraits.sizeType)]



  String = BasicString[cchar, CharTraits[cchar], Allocator[cchar]]


var test: String

proc npos[CharT, Traits, Alloc](self: BasicString[CharT, Traits, Alloc]):
  basicStringSizeType(self) =

  discard
