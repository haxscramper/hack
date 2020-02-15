import compiler/[
  modules, # implements the module handling

  ast, # type definitions of the abstract syntax tree (AST) and node
       # constructors

  astalgo, # algorithms for containers of AST nodes; converting the
           # AST to YAML; the symbol table

  passes, # implement the passes manager for passes over the AST

  llstream, # Low-level streams for high performance.

  modulegraphs, # This module implements the module graph data
                # structure. The module graph represents a complete
                # Nim project.

  idents, # implements a general mapping from identifiers to an
          # internal representation (PIdent)
  options
]

# Create cache of unique identifiers
let cache: IdentCache = newIdentCache()
# Project compilation configuration (name of the project, linked
# libraries etc.)
let config: ConfigRef = newConfigRef()

proc dummyOpen(graph: ModuleGraph; module: PSym): PPassContext = discard

proc registerToplevel(n: PNode): void =
  echo "registering toplevel node"

proc logASTNode(context: PPassContext, n: PNode): PNode =
  echo "called log ast node"
  result = n
  registerToplevel(n)
  debug(n)

proc displayAST*(program: string) =
  let g: ModuleGraph = newModuleGraph(cache, config)
  var m: PSym = makeStdinModule(g)
  incl(m.flags, sfMainModule)
  registerPass(g, makePass(open = dummyOpen, process = logASTNode))
  processModule(g, m, llStreamOpen(program))

# dumpTree:
#   proc hi(arg: int) =
#     ## Doc comment
#     echo "hi"


displayAST("""
proc hi(arg: int) =
  ## Doc comment
  echo "hi"
""")
