import common
import key_codes
import report


type
  KeyState* = enum
    kstChangedPressed ## Previously unmodified now pressed
    kstChangedReleased ## Previously pressed now released
    kstIdlePressed ## Previously pressed no change now
    kstIdleReleased ## Not pressed no change

  Key* = object
    state*: KeyState
    case isModifier*: bool
    of true: modif*: HIDModifiers
    of false: code*: KeyCode

  KeyGrid* = object
    keyGrid*: seq[seq[Key]]
    rowPins*: seq[int]
    colPins*: seq[int]


proc updateKey*(key: var Key, isPressed: bool): bool =
  ## Transition between key states based on whether or not it is
  ## pressed
  # IDEA write macro for function that perform role of state automata.
  # Provide DSL for writing transition rules (base it on some language
  # that is related to automata)
  result = false

  case key.state:
    of kstIdleReleased:
      if isPressed:
        key.state = kstChangedPressed
        result = true # Key is now pressed

    of kstIdlePressed:
      if not isPressed:
        key.state = kstChangedReleased
        result = true # Key is not released

    of kstChangedPressed:
      if isPressed:
        key.state = kstIdlePressed
        # Key is still pressed, no changes

    of kstChangedReleased:
      if not isPressed:
        # Key is still released, no changes
        key.state = kstIdleReleased


proc updateKeyGrid*(grid: var KeyGrid, matrixState: seq[seq[bool]]): bool =
  ## Determine whether or not any change ocurred in grid (based on
  ## matrixState). Return true if at least one key chaged. Grid will
  ## be modified to match new state (key states will be updated)
  var anyChanges = false

  for rowIdx, rowState in matrixState:
    for keyIdx, keyState in rowState:
      let isChanged = grid.keyGrid[rowIdx][keyIdx].updateKey(keyState)
      anyChanges = anyChanges or isChanged

  return anyChanges

proc createReport*(grid: KeyGrid): HIDReport =
  ## Generate report based on current grid status
  # TODO comment case-of for key state switching
  var keys: seq[KeyCode]
  for keyRow in grid.keyGrid:
    for key in keyRow:
      case key.state:
        of kstChangedReleased:
          if not key.isModifier:
            keys.add(ccKeyNone)

        of kstChangedPressed:
          if key.isModifier:
            result.modifiers.incl(key.modif)
          else:
            keys.add(key.code)

        else:
          discard

  for idx, keyCode in keys:
    if idx < result.keycodes.len:
      result.keycodes[idx] = keyCode
