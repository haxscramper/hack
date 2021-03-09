import sequtils, strformat
import hmisc/hpprint

type
  TreeIn = object
    val: int
    subt: seq[TreeIn]

  TreeOut = object
    val: string
    subt: seq[TreeOut]
    path: seq[int]


type
  DfsFrame[T, R] = object
    idx: int
    inSubt: seq[T]
    path: seq[int]
    subt: seq[R]

  DfsStack[T, R] = seq[DfsFrame[T, R]]

func mtr(val: int, subt: varargs[TreeIn]): auto =
  TreeIn(val: val, subt: toSeq(subt))

template last[T](stack: var seq[T]): var T = stack[^1]
template last[T](stack: seq[T]): T = stack[^1]
func getSubt(tree: TreeIn): seq[TreeIn] = tree.subt
func getCurrSubt[T, R](stack: DfsStack[T, R]): seq[T] =
  stack.last.inSubt[stack.last.idx].getSubt()

func getCurrPath[T, R](stack: DfsStack[T, R]): seq[int] =
  stack.last.path

func getCurr[T, R](stack: DfsStack[T, R]): T =
  stack.last.inSubt[stack.last.idx]

func makeDfsFrame[T, R](elems: seq[T], path: seq[int]): DfsFrame[T, R] =
  DfsFrame[T, R](idx: 0, inSubt: elems, path: path, subt: @[])


template dfsIterative[T, R](intree: T, expr: untyped): R =
  type Frame = DfsFrame[T, R]
  var stack: seq[Frame]
  var res: R
  stack.add makeDfsFrame[T, R](@[intree], @[])

  var cnt: int = 0
  block dfsLoop:
    while cnt < 20:
      if stack.last.idx == stack.last.inSubt.len:
        # Current toplevel frame reached the end
        let top = stack.pop
        let foldRes = (
          block:
            let
              it {.inject.} = stack.getCurr()
              path {.inject.} = top.path
              subt {.inject.} = top.subt

            expr
        )

        if stack.len == 1: # Popped last element in stack
          res = foldRes
          break dfsLoop
        else: # Folded frame, adding it to previous one's result
          inc stack.last.idx
          stack.last.subt.add foldRes

      else:
        stack.add makeDfsFrame[T, R](
          stack.getCurrSubt(), stack.getCurrPath() & @[stack.last.idx]
        )

  res


let intree = mtr(
  12,
  mtr(12, mtr(90), mtr(10)),
  mtr(9, mtr(8), mtr(888, mtr(8), mtr(-12)), mtr(222)),
)

let res = dfsIterative[TreeIn, TreeOut](
  intree,
  TreeOut(val: $it.val, subt: subt, path: path)
)

pprint intree
pprint res
