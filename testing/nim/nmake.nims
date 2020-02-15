#!/usr/bin/env nim
import strformat
import macros
import tables
import sets
import options
import strutils

type
  Task = object
    name: string
    descr: string
    deps: seq[string]

var taskRegistry: seq[Task]

proc taskProcName(name: string): string =
  "executeTask_" & name

var depTable: Table[string, seq[string]]
var tskNames: HashSet[string]
var execTable: Table[string, proc()]
var depDepth = 0

proc buildDependencyTable(): void =
  for tsk in taskRegistry:
    depTable[tsk.name] = tsk.deps
    tskNames.incl tsk.name

proc executeTaskByName(tsk: string): void =
  inc depDepth
  for dep in depTable[tsk]:
    if dep notin tskNames:
      error "Attempt to execute unknown dependecy " & dep
    else:
      executeTaskByName(dep)

  dec depDepth

  execTable[tsk]()

macro start(initName: string): untyped =
  # let initName = newCall(taskProcName(taskRegistry[0].name))
  buildDependencyTable()
  result = quote do:
    executeTaskByName(`initName`)

proc toStrSeq(node: NimNode): seq[string] =
  if node.kind == nnkPrefix and node[1].kind == nnkBracket:
    for lit in node[1]:
      result.add $lit

macro taskImpl(
  name, descr: string,
  deps: seq[string],
  body: untyped): untyped =

  let task = Task(
    name: $name,
    descr: $descr,
    deps: deps.toStrSeq()
  )

  taskRegistry.add task

  let runTask = ident(taskProcName($name))
  result = quote do:
    proc `runTask`(): void =
      echo "  ".repeat(depDepth) & "Running task " & `name`
      `body`

    execTable[`name`] = `runTask`

macro task(name, descr: string, body: untyped): untyped =
  quote do:
    taskImpl(name = `name`, descr = `descr`, deps = @[]):
      `body`

macro task(name, descr: string,
           deps: seq[string],
           body: untyped): untyped =
  quote do:
    taskImpl(name = `name`, descr = `descr`, deps = `deps`):
      `body`

task "dep", "test task":
  echo "asd d d dd d "

task "test", "test task", @["dep"]:
  echo "sfadf"


start("test")
