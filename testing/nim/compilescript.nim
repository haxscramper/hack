import nimcompiler/compiler/[
  astalgo,
  modulegraphs,
  idents,
  vmdef,
  sem,
  vm,
  llstream,
  options,
  condsyms,
  passes,
  modules,
  ast,
  pathutils,
  syntaxes,
  lineinfos
]

import os, threadpool, times

type
  Script* = ref object
      filename*: string
      moduleName*: string
      mainModule*: PSym
      graph*: ModuleGraph
      context*: PCtx
      watcher*: FlowVar[int]


var result: Script

let
  filename = "test.nims"
  scriptsDir = getAppDir()
  configRef = newConfigRef()
  identCache = newIdentCache()

configRef.errorMax = 120000
configRef.libpath = AbsoluteDir(scriptsDir & "/stdlib")
configRef.implicitIncludes.add(scriptsDir / "api.nim")

proc exposeScriptApi* (script: Script) =
    template expose (procName, procBody: untyped) {.dirty.} =
        script.context.registerCallback script.moduleName & "." & astToStr(procName),
            proc (a: VmArgs) =
                procBody

    expose add:
        # We need to use procs like getInt to retrieve the argument values from VmArgs
        # Instead of using the return statement we need to use setResult
        setResult(a,
            getInt(a, 0) +
            getInt(a, 1))

proc setupNimscript(graph: ModuleGraph) =
  graph.connectCallbacks()
  initDefines(configRef.symbols)
  defineSymbol(configRef.symbols, "nimscript")
  defineSymbol(configRef.symbols, "nimconfig")
  graph.registerPass(semPass)
  graph.registerPass(evalPass)


proc cleanupNimscript(graph: ModuleGraph) =
  # resetSystemArtifacts()
  initDefines(configRef.symbols)
  undefSymbol(configRef.symbols, "nimscript")
  undefSymbol(configRef.symbols, "nimconfig")
  clearPasses(graph)

# Populate result
result.new()
result.graph = newModuleGraph(identCache, configRef)
setupNimscript(result.graph)
result.filename = scriptsDir / filename
result.moduleName = filename.splitFile.name


echo "1"

result.mainModule = makeModule(result.graph, result.filename)


echo "2"


incl(result.mainModule.flags, sfMainModule)
result.context = newCtx(result.mainModule, identCache, result.graph)
result.context.mode = emRepl
echo "3"

# Expose API
result.exposeScriptApi()

# Set context
echo "4"

setupGlobalCtx(result.mainModule, result.graph)
registerAdditionalOps(result.context)
echo "5"

# Compile standard library
configRef.searchPaths.add(configRef.libpath)
configRef.searchPaths.add(AbsoluteDir configRef.libpath.string & " / pure")
echo getAppDir()
compileSystemModule(result.graph)

echo "6"
# Compile script as module

try:
  var parser: TParsers
  proc errorCallback(
    conf: ConfigRef;
    info: TLineInfo;
    msg: TMsgKind;
    arg: string): void =
      echo msg

  parser.parser.lex.errorHandler = errorCallback
  if not processModule(
    result.graph,
    result.mainModule,
    llStreamOpen(AbsoluteFile(result.filename), fmRead),
    parser
  ).bool:
      echo "Total fail"
      echo "Failed to process `", result.filename, "`"
  else:
    echo "seems ok"
except:
  echo "compilation failed"

echo "7"
# Cleanup
setupGlobalCtx(nil, result.graph)
cleanupNimscript(result.graph)


echo "9"

echo "done"
