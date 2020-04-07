.. contents::

   Well, let's not make anything overcomplicated. -- me, couple months
   ago



SBC software part for keyboard.

`keypad_reader.nim` is the main file. It repeatedly scans keypad grid
described in `config.json` and generates reports based on changed in
grid states.

Configuration file
==================

Configuration file for keyboard consists of four parts:

1. `"rows"` section - 2d array of key configurations. Each cell in the
   array can be either of two types: single string or array of
   configurations. For more details look into
   `report.decodeKeybindingConf` documentation.
2. `"modifiers"` - list of strings describing additional modifiers
   available for keyboard
3. `"colPins"` - list of integers desribing pins on the board that
   will be used for scanning columns
4. `"rowPins"` - list of integers describing pins on the board that
   will be used for scanning rows


Devnotes
========

keyboard algorithm:

- create grid from configuration file
- in loop:
  1) scan grid for changes
  2) for all newly triggered *or* already pressed keys get their
     exported modifiers
  3) for all newly triggered keys get their exported key codes
     - if found any key with immediate export chord finish loop and
       send report
     - for any non-final key add it's exported key to output
  4) Scan exported modifiers and retrieve only those who are present
     on regular keyboard (ctrl, shift, meta, alt). Convert them to hid
     modifiers
  5) Combine exported wid modifiers and collected exported keys.
     Generate final report and return it.

Each key has two triggers: 'on press' and 'on release'. Each trigger
is a table with mapping between current modifiers and corresponding
export results. There are two types of possible export results:
single-action 'non-final' and multi-action 'final' ones.

Each key result contains all it's exported keys: if you want to make
'ctrl' key you need to map `"default"` to `KeyResult(modifiers:
toHashSet(["ctrl"]), isFinal: false)`.

..
   For single-chord keys both `KeyPress.modifiers` and
   `KeyResult.modifiers` are used to generate final combination (it was
   made to enable support for )

For non-final keys only modifiers accumulated from
`KeyResult.modifiers` are used.

For final keys only actions in `KeyResult.chords` are executed - no
modifications of generated report occurs.

If you want to:

- **make ctrl modifier**: map `"default"` to `KeyResult(modifiers:
  toHashSet(["ctrl"]), isFinal: false)`
- **make custom modifier called 'math'**: map `"default"` to
  `KeyResult(modifiers: toHashSet(["math"]), isFinal: false)`
- **make '>' insert unicode function map arrow with 'math' modifier using
  compose key combination**: have following mapping in `onRelease` table:
  - `"default" -> ">"`
  - `"math" -> "<PAUSE>" m s m`. This keybindings assumes the
    following:
    - You have `<Multi_key> <m> <s> <m> : "↦" # *M*apsto` in your
      `~/.Xcompose` file. Adding this binding will take effect only
      for new X clients.
    - you are using "<pause>" as compose key (can be enabled using
      `setxkbmap -option compose:paus` - don't forget that this takes
      effect only for *new* X clients too)
- **create key that switches languages only when pressed**, assuming
  language switching is done using meta+space (can be enabled using
  `setxkbmap -option 'grp:win_space_toggle'`): map "default" to export
  "meta" and trigger "spc" on both `onRelease` and `onPress`. It is
  important to make this key 'final'. This won't interfere with any
  other keybindings as final keys are exported immediately when they
  are triggered. Because this binding does not export any modifiers it
  will not alter functions of other bindings in any way.


Electronics
===========

Particular scanning order for grid is dictated my underlying hardware.
