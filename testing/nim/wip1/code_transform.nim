import compiler/[parser, llstream, idents, options,
                 pathutils, astalgo, msgs,
                 ast, renderer]

var config: ConfigRef = newConfigRef()
var p: TParser

openParser(p,
           fileInfoIdx(config, AbsoluteFile "/tmp"),
           llStreamOpen("let a = b\nlet b = c"),
           newIdentCache(),
           config)

let node = parseAll(p)
debug(node)
echo "hello"
echo $node
