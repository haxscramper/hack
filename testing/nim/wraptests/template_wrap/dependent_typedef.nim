const hasRepr = false
when hasRepr:
  import std/macros
  import hnimast/hast_common
  import hmisc/algo/[hstring_algo, halgorithm]
  import hmisc/types/[colorstring]
  import std/[sequtils, strutils, strformat]

  proc typedRepr*(
      pnode: NimNode, colored: bool = true,
      indexed: bool = false, maxdepth: int = 120
    ): string =

    proc aux(n: NimNode, level: int, idx: seq[int], expandSym: bool): string =
      let pref =
        if indexed:
          idx.join("", ("[", "]")) & "    "
        else:
          "  ".repeat(level)

      if isNil(n):
        return pref & toRed("<nil>", colored)

      if level > maxdepth:
        return pref & " ..."

      result &= pref

      let name = ($n.kind)[3..^1]
      result.add case n.kind:
        of nnkSym: toBlue(name, colored)
        of nnkProcKinds: toRed(name, colored)
        of nnkEmpty: toGreen(name, colored)
        else: name

      case n.kind:
        of nnkStrKinds:
          result &= " \"" & toYellow(n.getStrVal(), colored) & "\""

        of nnkIntKinds:
          result &= " " & toBlue($n.intVal, colored)

        of nnkFloatKinds:
          result &= " " & toMagenta($n.floatVal, colored)

        of nnkIdent:
          result &= " " & toGreen(n.getStrVal(), colored)

        of nnkSym:
          if expandSym:
            let iinfo = n.getTypeImpl().lineInfoObj()
            result &= " " & toYellow($iinfo.line & ":" & $iinfo.column, colored)
            result &= "\n"
            result &= "" & aux(n.getTypeImpl(), level + 1, idx & 0, false)

          else:
            result &= " " & toItalic(n.repr(), colored)

        of nnkCommentStmt:
          let lines = split(n.strVal(), '\n')
          if lines.len > 1:
            result &= "\n"
            for idx, line in pairs(lines):
              if idx != 0:
                result &= "\n"

              result &= pref & toYellow(line)

          else:
            result &= toYellow(n.strVal())

        else:
          if n.len > 0:
            result &= "\n"

          for newIdx, subn in n:
            result &= aux(subn, level + 1, idx & newIdx, expandSym)
            if newIdx < n.len - 1:
              result &= "\n"

    return aux(pnode, 0, @[], true)

  macro dumpTyped(body: typed): untyped =
    echo typedRepr(body)

type
  StdCharTraits[CharT] {.header: "<string>", importcpp: "std::char_traits".} = object

  StdSizeT {.importcpp: "std::size_t".} = cint

  StdAllocator[T] {.header: "memory", importcpp: "std::allocator".} = object

  AllocTraits[A] = object

  BasicString[CharT, Traits, Alloc] {.header: "<string>", importcpp: "std::basic_string".} = object

  String = BasicString[cchar, StdCharTraits[cchar], StdAllocator[cchar]]


proc rebindOther[T, Tp](
  self: typedesc[StdAllocator[T]], tp: typedesc[Tp]): StdAllocator[Tp] =
  # Wrapper for `rebind` typedef inside of `std::allocator<T>`
  # `template<typename _Tp1> struct rebind { typedef allocator<_Tp1> other; };`
  # 'returns' new allocator type where `T` is replaced with `Tp`
  discard


proc rebindOther[Alloc, Tp](self: typedesc[AllocTraits[Alloc]], tp: typedesc[Tp]):
  auto =
  # Declared in `ext/alloc_traits.h` (trait wrapper around default allocator)
  #
  #```
  # template<typename _Tp>
  #   struct rebind
  #   { typedef typename _Alloc::template rebind<_Tp>::other other; };
  #```
  if false:
    var tmp: typeof rebindOther(Alloc, Tp)
    return tmp

proc charAllocType(Alloc: typedesc, CharT: typedesc): auto =
  if false:
    var tmp: typeof rebindOther(AllocTraits[Alloc], CharT)
    return tmp

proc allocTraits(CharAllocType: typedesc): auto =
  if false:
    var tmp: AllocTraits[CharAllocType]
    return tmp

proc sizeType[T](arg: typedesc[StdAllocator[T]]): StdSizeT =
  discard

proc sizeType[Alloc](arg: typedesc[AllocTraits[Alloc]]): auto =
  if false:
    var tmp: typeof sizeType(Alloc)
    return tmp

proc sizeType[C, T, A](self: BasicString[C, T, A]): auto =
  # Series of typedefs (most of them private) for getting size type of an
  # allocator
  #
  # `typedef typename __gnu_cxx::__alloc_traits<_Alloc>::template rebind<_CharT>::other _Char_alloc_type;`
  # `typedef __gnu_cxx::__alloc_traits<_Char_alloc_type> _Alloc_traits;`
  # `typedef typename _Alloc_traits::size_type		size_type;`
  if false:
    type charAllocType_T = typeof rebindOther(AllocTraits[A], C)
    type allocTraits_T = typeof allocTraits(charAllocType_T)
    var tmp: typeof(sizeType(allocTraits_T))

    return tmp

proc npos[CharT, Traits, Alloc](self: BasicString[CharT, Traits, Alloc]): auto {.noinit.} =
  # Wrap static field `::npos`
  {.emit: "\n#if 0".}
  if false:
    var tmp: typeof sizeType(self)
    return tmp

  {.emit: "\n#endif".}


  {.emit: "return std::basic_string<`CharT`, `Traits`, `Alloc`>::npos;"}

var test: String

echo typeof(test.npos())
echo test.npos()
