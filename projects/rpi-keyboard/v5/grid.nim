import common

type
  HIDModifiers = enum
    hmLeftCtrl = 0
    hmLeftShift = 1
    hmLeftAlt = 2
    hmLeftSuper = 3

    hmRightCtrl = 4
    hmRightShift = 5
    hmRightAlt = 6
    hmRightSuper = 7

type
  KeyState = enum
    kstChangedPressed ## Previously unmodified now pressed
    kstChangedReleased ## Previously pressed now released
    kstIdlePressed ## Previously pressed no change now
    kstIdleReleased ## Not pressed no change

  Key = object
    state: KeyState
    case isModifier: bool
    of true: modif: HIDModifiers
    of false: code: KeyCode

  KeyGrid = object
    keyGrid: seq[seq[Key]]
    rowPins: seq[int]
    colPins: seq[int]
