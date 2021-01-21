{.passl: "-ltree-sitter".}
{.compile: "parser.c".}

import typespec_wrapper

let parser = newTypeSpecParser()

let str = "seq[int]"

let tree = parser.parseString(str)
echo tree.treeRepr(str)
