import hmisc/wrappers/wraphelp

{.compile: "cyg_profile.c".}
{.passc: "-finstrument-functions".}
{.passl: "-ldl".}
{.passc: "-rdynamic".}

const h = "cyg_profile.h"

type
  DlInfo {.importc: "Dl_info", header: "<dlfcn.h>".} = object
    dli_fname: cstring
    dli_fbase: pointer
    dli_sname: cstring
    dli_saddr: pointer

proc dladdr(fAddr: pointer, info: ptr DlInfo) {.importc: "dladdr", header: "<dlfcn.h>".}

type
  EventKind {.importc: "enum event_kind", header: h.} = enum
    ek_enter
    ek_exit

  Event {.importc: "struct event", header: h.} = object
    kind: EventKind
    eFunc {.importc: "func".}: pointer
    caller: pointer

  EventList {.importc: "struct int_list", header: h.} = object
    size: cint
    value: PUarray[Event]

proc disableProfile() {.importc: "__cyg_disable_profile", header: h.}
proc getProfile(): ptr EventList {.importc: "__cyg_get_profile", header: h.}



proc main() =
  echo "ok"
  disableProfile()

  let prof = getProfile()
  echo prof.size

  for idx, event in pairs(prof.value, prof.size):
    var i: DlInfo
    dladdr(event.eFunc, addr i)
    echo idx, " ", event.kind, " ", i.dliSname, " @ ", cast[int](event.eFunc)

main()
