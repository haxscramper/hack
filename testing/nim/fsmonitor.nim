import os
import  FSMonitor   # needed for FSMonitor class
import  asyncio

# this is callback handler for dispatcher
proc monitorEvent(m: FSMonitor; ev: MonitorEvent) =
  echo (ev)

var monitor = newMonitor()
echo monitor.add("/tmp")

var dispatcher = newDispatcher ()
dispatcher.register(monitor, monitorEvent)

while true:
  if not dispatcher.poll(): break
