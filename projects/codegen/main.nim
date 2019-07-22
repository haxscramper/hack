import strformat
import sequtils
import strutils

type
  TypeProps = enum
    pStatic
    pConst
    pRef
    pPtr

  Type = object
    name: string
    props: seq[TypeProps]

  Var = object
    vName: string
    vType: Type

  InhType = enum
    public
    private
    protected


type
  AcnKind = enum
    acnEnum
    acnClass
    acnFunction
  Acn = ref object
    name: string
    case kind: AcnKind
      of acnEnum:
        vFields: seq[Var]
      of acnClass:
        parents: seq[(InhType, Type)]
        body: seq[Acn]
      of acnFunction:
        restype: Type
        args: seq[(Var)]

type
  CNodeKind = enum
    cnkLeaf
    cnkNode
  CNode = ref object
    case kind: CNodeKind
      of cnkLeaf:
        code: string
      of cnkNode:
        under: seq[CNode]


proc class_to_cnode(class: Acn): CNode =
  CNode(
    kind: cnkNode,
    under: @[
      CNode(
        kind: cnkLeaf,
        code: &"{class.name}"
      )])

proc cnode_to_string(cnode: CNode): string =
  case cnode.kind:
    of cnkLeaf: cnode.code
    of cnkNode: join(map(cnode.under, cnode_to_string), "\n")


echo cnode_to_string(class_to_cnode(Acn(
  kind: acnClass,
  name: "QSTodo"
)))
