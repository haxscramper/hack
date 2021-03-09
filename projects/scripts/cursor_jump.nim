import x11/[xlib, xutil, x],
       std/[os, sequtils, segfaults, strutils, strformat]

proc createWindowAt(
  display: PDisplay, screen: cint,
  posX, posY: int, sizeX, sizeY: int,
  attrs: XSetWindowAttributes): Window =
  var window_attr = attrs
  XCreateWindow(display, XRootWindow(display, screen),
      posX.cint,
      posY.cint,
      sizeX.cuint,
      sizeY.cuint,
      0,
      DefaultDepth(display, screen),
      CopyFromParent,
      DefaultVisual(display, screen),
      CWOverrideRedirect,
      addr window_attr
  )

proc enumWindows(d: PDisplay, topw: Window, level: int = 0) =
  var
    nchild: cuint
    children: PWindow
    parent: Window
    root: Window

  var text: XTextProperty
  discard XGetWMName(d, topw, addr text)
  var name: cstring
  discard XFetchName(d, topw, addr name)
  let r = ("  ".repeat(level), &"id={topw:x}, XFetchName=\"$2\", XGetWMName=\"$3\"" % [
    $topw, (if name == nil: "(no name)" else: $name), $text.value
  ])

  discard XQueryTree(d, topw,
    addr root,
    addr parent,
    addr children,
    addr nchild
  )

  var arr = cast[ptr UncheckedArray[Window]](addr children)

  var attrs: XWindowAttributes
  for ch in 0 ..< nchild:
    if arr[ch] != 0 and arr[ch] != topw:
      echo &"0x{arr[ch]:x}"
      try:
        enumWindows(d, arr[ch], level + 1)
      except NilAccessDefect:
        discard
      finally:
        discard
        # discard XFree(children)



var
  size: cint = 200
  step: cint = 20
  speed: cint = 30
  lineWidth: cint = 2
  colorName: string = "red"

let display_name = getenv("DISPLAY")

let
  display = XOpenDisplay(display_name)
  screen = DefaultScreen(display)

# Get the mouse cursor position
var
  win_x, win_y, root_x, root_y: cint
  mask: cuint


var child_win, root_win: Window

discard XQueryPointer(display, XRootWindow(display, screen),
    addr child_win,
    addr root_win,
    addr root_x,
    addr root_y,
    addr win_x,
    addr win_y,
    addr mask
)

var  window_attr: XSetWindowAttributes
window_attr.override_redirect = 1;
let window = display.createWindowAt(
  screen,
  (root_x - size div 2).int,
  (root_y - size div 2).int,
  size.int,
  size.int,
  window_attr
)

discard XMapWindow(display, window)
discard XStoreName(display, window, "find-cursor")

var class = XAllocClassHint()
class.res_name = "find-cursor"
class.res_class = "find-cursor"
discard XSetClassHint(display, window, class)
discard XFree(class)

var e: XEvent
e.xclient.theType = ClientMessage
e.xclient.message_type = XInternAtom(display, "_NET_WM_STATE", cint(0))
e.xclient.display = display
e.xclient.window = window
e.xclient.format = 32
e.xclient.data.l[0] = 1
e.xclient.data.l[1] = clong XInternAtom(display, "_NET_WM_STATE_STAYS_ON_TOP", cint(0))
discard XSendEvent(
  display, XRootWindow(display, screen), cint(0), SubstructureRedirectMask, addr e)

discard XRaiseWindow(display, window)
discard XFlush(display)

# Prepare to draw on this window
var values: XGCValues
values.graphics_exposures = cint(0)

var valuemask: culong
var gc: GC = XCreateGC(display, window, valuemask, addr values)

var colormap = DefaultColormap(display, screen)
var color: XColor
discard XAllocNamedColor(display, colormap, color_name, addr color, addr color)
discard XSetForeground(display, gc, color.pixel)
discard XSetLineAttributes(display, gc, line_width.cuint,
                           LineSolid, CapButt, JoinBevel)

iterator range[I: SomeInteger](start, finish: I, step: I = 0): I =
  var start = start
  while start < finish:
    yield start
    start += step

for i in range(0.cint, size, step):
    discard XDrawArc(display, window, gc,
        size div 2 - i div 2,
        size div 2 - i div 2,
        i.cuint,
        i.cuint,
        0, 360 * 64
    );

    discard XSync(display, cint(0))
    sleep(speed)


# IDEA set global variable for all exceptions and write `xcall` proc
# that will execute X code and then check for exception. If there is a
# stored exception - reset counter and raise it. Otherwise ignore.

proc errHandler(d: PDisplay, ev: ptr XErrorEvent): cint {.cdecl.} =
  var str = " ".repeat(80)
  discard XGetErrorText(d, ev.theType, str.cstring, str.len.cint)
  str = str.strip()
  raiseAssert fmt("""

X Error of failed request:  BadWindow (invalid Window parameter)
  Major opcode of failed request:  {ev.requestCode} ({str})
  Resource id in failed request:  {ev.resourceId:x}
  Serial number of failed request:  {ev.serial}
  Current serial number in output stream:  {ev.serial}
""")

# discard XSetErrorHandler(errHandler)


# enumWindows(display, XDefaultRootWindow(display))

discard XFreeGC(display, gc)
discard XCloseDisplay(display)
