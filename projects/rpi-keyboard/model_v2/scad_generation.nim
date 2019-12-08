## This module is used to generate openscad files for keyboard block.

import geometry
import geometry_generation
import keyboard

proc toSCAD*(blc: Block): string =
  "temp;"
