import std/[pegs]

if "1" =~ peg"""
Main <- {Digit}
Digit <- \d
""":
  echo matches
