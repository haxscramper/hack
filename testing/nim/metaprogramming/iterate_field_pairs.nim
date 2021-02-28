import sugar, strutils, sequtils, strformat

#===========================  implementation  ============================#

import private_fields

for field, val in U().fieldPairs():
  echo field, ": ", typeof val
