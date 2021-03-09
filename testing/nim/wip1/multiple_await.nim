import hmisc/fsmonitor
import hmisc/helpers
import asyncnet, asyncdispatch, strutils
import json
import deques
import options
import macros
import colechopkg/lib

type
  InEventKind* = enum
    iekSocketMessage
    iekFileEvent

  InEvent* = object
    case kind*: InEventKind
      of iekSocketMessage:
        message*: string
      of iekFileEvent:
        fileEv*: MonitorEvent

  MChangeKind* = enum
    addRelativeFile

  MonitorChange* = object
    case kind*: MChangeKind
      of addRelativeFile:
        file*: string

var mchangeQueue = initDeque[MonitorChange]()



macro while_let(head, body: untyped): untyped =
  # TODO chech for assgn and parenthesis
  let identNode = head[0][0]
  let generatorNode = head[0][1]

  result = quote do:
    var optVal  = `generatorNode`
    while optVal.isSome():
      let `identNode` {.inject.} = optVal.get()
      `body`
      optVal = `generatorNode`


proc addMonitorChange(change: MonitorChange): void =
  mchangeQueue.addLast(change)

proc getMonitorChange(): Option[MonitorChange] =
  if mchangeQueue.len > 0:
    mchangeQueue.popFirst()

proc patchMonitor(monitor: var FSMonitor): void =
  while_let (change = getMonitorChange()):
    case change.kind:
      of addRelativeFile:
        monitor.add(change.file, {MonitorModify})
        echo "added relative file to monitor"




proc handleFileModified(event: MonitorEvent): void =
  echo "file: ", event.name, " was modified"

proc handleCommand(command: JsonNode): void =
  echo "processing command: "
  echo command.pretty
  for key, value in command:
    case key:
      of "add-relative-files":
        echo "request to add relative file to monitor"
        for file in value.getElems:
          echo "reqested to add file: ", file.getStr
          addMonitorChange(
            MonitorChange(
              kind: addRelativeFile,
              file: file.getStr
          ))

          echo "added monitor change"

proc processEvent(event: InEvent): void =
  case event.kind:
    of iekSocketMessage:
      try:
        let json = parseJson(event.message)
        if json.kind == JObject:
          handleCommand(json)
        else:
          ceUserError0 "accepted message can be parsed as json but contains invalid data"
      except JsonParsingError:
        # TODO write to log file/socket error message
        ceUserError0 "Input string cannot be parsed as json: " & event.message
        #discard
    of iekFileEvent:
      case event.fileEv.kind:
        of MonitorModify: handleFileModified(event.fileEv)
        else: discard


proc listenEvents(client: AsyncSocket) {.async.} =
  while true:
    let line = await client.recvLine()

    if line.len == 0:
      ceUserWarn("Zero-length line, terminathing connection")
      break
    else:
      ceUserInfo0 "Processing line " & $line

    processEvent(InEvent(kind: iekSocketMessage, message: line))

proc startListener() {.async.} =
  echoi "starting server"
  var server = newAsyncSocket()
  server.setSockOpt(OptReuseAddr, true)
  server.bindAddr(Port(12345))
  server.listen()
  while true:
    let client = await server.accept()

    let clAddr = client.getLocalAddr()
    let port: Port = clAddr[1]
    let host: string = clAddr[0]
    #echo port
    # echo host
    # ceUserLog0("New connection from " & ($host) & ":" & ($port))

    asyncCheck listenEvents(client)

proc startMonitor() {.async.} =
  echoi "starting monitor"
  var monitor = newMonitor()
  for file in @["test.tmp"]:
    monitor.add(file, {MonitorAll})

  while true:
    patchMonitor(monitor)
    # IDEA to apply patches to monitor on wait we could just `touch`
    # any of the watched files and monitor will generate event
    let events: seq[MonitorEvent] = await monitor.read()
    for ev in events:
      processEvent(InEvent(kind: iekFileEvent, fileEv: ev))

when isMainModule:
  asyncCheck startListener()
  echo "started socket listener"
  asyncCheck startMonitor()
  echo "started monitor"
  runForever()
