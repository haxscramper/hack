{.compile: "parser.c".}
{.passl: "-ltree-sitter".}

import haxlink_wrapper
import hparse/htreesitter/htreesitter
import std/[strformat, strutils]

var parser = newHaxLinkParser()

var str = "std/sugar/a::c::method(int, seq[string, float]).arg1"



let tree = parser.parseString(str)
echo tree.treeRepr(str)
