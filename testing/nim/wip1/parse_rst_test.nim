import hpprint
import packages/docutils/[rst, rstast, rstgen]

proc parseRstString(s: string): PRstNode =
  const filen = "input"
  var dummyHasToc = false
  result = rstParse(s, filen, 0, 1, dummyHasToc, {})

let testStr = """

*eee 12*

e df sfa ee


"""


let node = parseRstString(testStr)
pprint node

# echo rstToHtml(testStr, {}, defaultConfig())

proc myFindFile(filename: string): string =
  # we don't find any files in online mode:
  result = ""

const filen = "input"
var d: RstGenerator
initRstGenerator(d, outHtml, defaultConfig(), filen, {}, myFindFile,
                 rst.defaultMsgHandler)

var dummyHasToc = false
var res = ""
renderRstToOut(d, node, res)

echo res
