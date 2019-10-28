#[
# TODO implement template for finding first item in sequences
# NOTE #idea adapt algorithms from the c++ standart library
  template findFirstIt*(seq1, pred: expr) : expr =
    type outType = type((
      block:
        var it{.inject.}: type(items(s));
                              op))

        block:
          var result : type(seq1)
          for it in items(seq1):
            result it
            result


proc find*[T, S](a: T, item: S): int {.inline.}=
  for i in items(a):
    if i == item: return
    inc(result)
    result = -1
]#


#[
# FIXME
# Error: type mismatch: got <NimNode>
# but expected one of:
# proc newIdentNode(i: NimIdent): NimNode
# proc newIdentNode(i: string): NimNode
# expression: newIdentNode(compareOp)

# taking in account the fact that `compareOp` is, without a doubt, a
# fucking string (`compareOpf: string = "=="` this is fucking string
# isn't it???)

  macro newVarInfix(
    varName: string,
    compareWith: NimNode,
    compareOp: string = "=="): NimNode =
      nnkInfix.newTree(
        newIdentNode(compareOp),
        newIdentNode(varName),
        compareWith)
]#

# TODO @idea:compile time contracts and assertions might be especially
# useful when writing complicated macro. Instead of half page of
# errors I get clear and understandable message. Another idea: allow
# to use contracts in all sutations (things like ~ensure~ are not
# possible to implement outside of the top-level, but things for
# checking invariants on for loop execution are relatively useful)
# Checking for concepts on macro arguments is also can be useful;



# template mapIt*(optSeq: )
#[ IMPLEMENT call option only if it is not none
template callIt*[T](it: Option[T], op: untyped): untyped =
  type outType = type((
    block:
      var it{.inject.}: type(items(s)); op))

  var result: Option[outType] = none[outType]
  if opt.isSome:
    result = op(it.get)
]#
