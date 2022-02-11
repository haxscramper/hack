type
  VmDataKind = enum
    vdkInt
    vdkNode
    vdkSeq
    vdkPtr

  PNode = ref object

  VmAddr* = distinct uint

  VmData* = object
    case kind*: VmDataKind
      of vdkInt:
        intVal*: BiggestInt

      of vdkNode:
        nodeVal*: PNode

      of vdkSeq:
        seqVal*: VmAddr

      of vdkPtr:
        ptrVal*: VmAddr


  VmOpc* = enum
    opcAdd
    opcAppend
    opcAddr

var mem: seq[VmData]
