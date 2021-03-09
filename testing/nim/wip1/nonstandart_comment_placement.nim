# NOTE does not work; fails at parsing stage

import hnimast
import compiler/ast
import hpprint

let node = parsePNodeStr(
"""
proc hello(a: int, ##[documentation comment]##): void =
  discard
""")

pprint node
