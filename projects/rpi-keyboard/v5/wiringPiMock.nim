## Simulate connected pins using ngspice simulation

import ../../ngspice_digital_read/[ngspice, parse_netlist]

import hmisc/defensive
import strutils

initDefense(prefix = "NGS")

var document: NGDocument
var silent: bool = false

proc ngReadCircuit(path: string): void =
  document = parseNGDoc(path)

proc ngAddIncluded(files: seq[string]): void =
  document.included.add files

proc ngSilentSimulation(arg: bool = true): void =
  silent = arg

proc piSetup*(): cint =
  result = 0

  ngSpiceInit(
    printfcn = (
      proc(msg: string, a2: int): int =
        if (not msg.startsWith("stdout *")) and (not silent):
          showLog(msg)
    ).addPtr(),
  )

proc piSetupGPIO*(): cint = 0
proc piSetupPhys*(): cint = 0
proc piSetupSys*(): cint = 0
proc piPinModeOutput*(pin: cint) = discard
proc piPinModeInput*(pin: cint) = discard
proc piPinModeGPIO*(pin: cint) = discard
proc piPinModePWM*(pin: cint) = discard
proc piDigitalPWM*(pin, value: cint) = discard
proc piDigitalWrite*(pin, value: cint) = discard
proc piDigitalRead*(pin: cint): cint = discard
proc piPullOff*(pin: cint) = discard
proc piPullDown*(pin: cint) = discard
proc piPullUp*(pin: cint) = discard
proc analogWrite*(pin, value: cint) = discard
proc analogRead*(pin: cint): cint = 0
