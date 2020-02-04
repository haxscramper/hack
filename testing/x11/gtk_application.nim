import gintro/[gtk, glib, gobject, gio]
import hotcodereloading

import nim_module

proc saveCb(action: SimpleAction; v: Variant) =
  echo "saveCb 2"

proc appActivate(app: Application) =
  let window = newApplicationWindow(app)

  let action = newSimpleAction("save")
  discard action.connect("activate", saveCB)
  window.actionMap.addAction(action)

  let button = newButton()
  button.label = "Save"
  window.add(button)

  button.setActionName("win.save")
  setAccelsForAction(app, "win.save", "<Control><Shift>S")
  showAll(window)

proc main =
  echo "Application starting"
  echo "From module: ", getCompileDate()
  let app = newApplication("org.gtk.example")
  connect(app, "activate", appActivate)
  discard run(app)

main()
