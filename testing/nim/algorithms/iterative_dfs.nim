import sequtils, strformat

type
  TreeIn = object
    val: int
    subt: seq[TreeIn]

  TreeOut = object
    val: string
    subt: seq[TreeOut]


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

func getCurr[T, R](stack: DfsStack[T, R]): T =
  stack.last.inSubt[stack.last.idx]

func makeDfsFrame[T, R](elems: seq[T], path: seq[int]): DfsFrame[T, R] =
  DfsFrame[T, R](idx: 0, inSubt: elems, path: path, subt: @[])


template dfsIterative[T, R](intree: T, expr: untyped): R =
  type Frame = DfsFrame[T, R]
  var stack: seq[Frame]
  var res: R
  stack.add makeDfsFrame[T, R](@[intree], @[0])

  var cnt: int = 0
  block dfsLoop:
    while cnt < 20:
      inc cnt
      if stack.last.idx < stack.last.inSubt.len:
        echo "Stack has ", stack.len, " frames, last index is ",
         stack.last.idx, " number of subterms for last: ",
         stack.last.inSubt.len

        let subt = stack.getCurrSubt()

        if subt.len == 0:
          echo "Reached leaf"
        else:
          # Push new stack frame
          stack.add makeDfsFrame[T, R](
            subt, @[1] #[ IMPLEMENT ]#
          )

        inc stack.last.idx

      else:
        echo "Folding stack"
        # Fold stack frame
        echo "Stack len now is: ", stack.len
        let top = stack.pop
        let foldRes = (
          block:
            let
              it {.inject.} = stack.getCurr()
              path {.inject.} = stack.last.path
              subt {.inject.} = top.subt

            expr
        )

        echo "removed element from stack top"
        echo "New stack length: ", stack.len

        if stack.len == 0:
          res = foldRes
          break dfsLoop
        else:
          stack.last.subt.add foldRes

  res


let intree = mtr(
  12,
  mtr(12, mtr(90), mtr(10)),
  mtr(9, mtr(8), mtr(888, mtr(8), mtr(-12)), mtr(222)),
)

let res = dfsIterative[TreeIn, TreeOut](
  intree,
  TreeOut(val: $it, subt: subt)
)

echo res
