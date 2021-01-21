{.compile: "parser.c".}
{.passl: "-ltree-sitter".}

import haxlink_wrapper
import hparse/htreesitter/htreesitter
import std/[strformat, strutils]

var parser = newHaxLinkParser()

var str = "std/sugar/a::c::method(cxx<float>, seq[string]).arg1"



let tree = parser.parseString(str)
echo tree.treeRepr(str)
