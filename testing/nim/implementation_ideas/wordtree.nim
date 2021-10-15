import hmisc/core/all
import std/[strutils]

type
  WordKind* = enum
    wkText
    wkNewline
    wkPunctuation
    wkSpace
    wkMarkup
    wkVerbatim

  Word*[Text] = object
    text*: Text
    forceNl: bool
    isBreaking: bool
    kind*: WordKind

  WordTreeKind = enum
    wtConcat
    wtSentence
    wtParagraph

    wtDescItem
    wtExplainItem
    wtStrong
    wtEmphasis
    wtUnderscore
    wtCode
    wtQuoted
    wtWord
    wtRaw
    wtAnyOf
    wtAllOf
    wtItemList


  WordTree*[Text] = ref object
    kind: WordTreeKind
    subnodes: seq[WordTree[Text]]
    word: Word[Text]

  TextWord = Word[string]
  TextTree = WordTree[string]

func len*[T](t: WordTree[T]): int =
  t.subnodes.len

# func `[]`*[T](t: WordTree[T], idx: int): WordTree[T] =
#   t.subnodes[idx]

func tree*[T](
    kind: WordTreeKind,
    subnodes: varargs[WordTree[T]]): WordTree[T] =
  WordTree[T](kind: kind, subnodes: @subnodes)

func tree*[T](
    kind: WordTreeKind, word: Word[T],
    subnodes: varargs[WordTree[T]]): WordTree[T] =
  WordTree[T](kind: kind, word: word, subnodes: @subnodes)



func initWord*(word: var string, s: string) = word = s

func word*[T](s: string): WordTree[T] =
  var text: T
  initWord(text, s)
  tree(wtWord, Word[T](text: text))

func textWord*(s: string): TextTree = word[string](s)

func `$~~`*(s: string): TextTree =
  tree(wtSentence, word[string](s))

proc `$~`*[T](arg: T): TextTree = textWord($arg)
proc `$~`*[T](arg: seq[T]): seq[TextTree] =
  for item in arg:
    result.add textWord($item)

proc `$~`*[R, T](arg: array[R, T]): seq[TextTree] =
  for item in arg:
    result.add textWord($item)

func wordMap*[O, T](
    obj: openarray[O], fmap: proc(obj: O): WordTree[T]): seq[WordTree[T]] =
  for item in obj:
    result.add fmap(item)

func wordMap*[O](obj: openarray[O]): seq[TextTree] =
  for item in obj:
    result.add textWord($item)

func strong*[T](words: varargs[WordTree[T]]): WordTree[T] = tree(wtStrong, @words)
func anyOf*[T](words: varargs[WordTree[T]]): WordTree[T] = tree(wtAnyOf, @words)
func wantGot*[T](want, got: WordTree[T]): WordTree[T] =
  tree(wtConcat, word[T]("expected "),
       want, word[T](", but got "), got)

func gotWant*[T](got, want: WordTree[T]): WordTree[T] =
  tree(wtConcat, word[T]("got "),
       got, word[T](", but expected "), want)

func wantGot*[T](thing, want, got: WordTree[T]): WordTree[T] =
  tree(wtConcat, thing, word[T](" has "),
       got, word[T](", but expected "), want)

func desc*[T](desc: string, item: WordTree[T]): WordTree[T] =
  tree(wtDescItem, word[T](desc), item)

func expl*[T](item, explain: WordTree[T]): WordTree[T] =
  tree(wtExplainItem, item, explain)

func sent*[T](words: varargs[WordTree[T]]): WordTree[T] =
  tree(wtSentence, @words)

func par*[T](words: varargs[WordTree[T]]): WordTree[T] =
  tree(wtParagraph, @words)

func many*[T](onMany, onOne: string, tree: WordTree[T]): WordTree[T] =
  if len(tree) == 0:
    result = word[T](onOne)

  else:
    result = tree(wtConcat, word[T](onMany), tree)



func quote*[T](words: varargs[WordTree[T]]): seq[WordTree[T]] =
  for word in words:
    result.add tree(wtQuoted, word)

func `[]`*[T](tree: WordTree[T], idx: int): WordTree[T] = tree.subnodes[idx]

func toStr*[T](tree: WordTree[T]): string =
  case tree.kind:
    of wtStrong, wtConcat, wtDescItem,
       wtSentence, wtQuoted:
      let push =
        case tree.kind:
          of wtStrong: "*"
          of wtQuoted: "'"
          else: ""

      result.add push
      for sub in tree.subnodes:
        result.add toStr(sub)
      result.add push

      case tree.kind:
        of wtSentence: result.add ". "
        else: discard

    of wtParagraph:
      for idx, item in pairs(tree.subnodes):
        if item.kind == wtSentence:
          var pos = result.len
          result.add toStr(item)
          if result[pos] in {'a' .. 'z'}:
            result[pos] = toUpperAscii(result[pos])


        else:
          result.add toStr(item)

    of wtWord:
      result.add $tree.word.text

    of wtExplainItem:
      result.add toStr(tree[0])
      result.add " ("
      result.add toStr(tree[1])
      result.add ")"

    of wtAnyOf:
      case tree.subnodes.len:
        of 0:
          raise newArgumentError("Empty 'any of' list")

        of 1:
          return toStr(tree.subnodes[0])

        of 2:
          result = &["either ", toStr(tree[0]), " or ", toStr(tree[1])]

        else:
          for idx, sub in pairs(tree.subnodes):
            if idx == tree.subnodes.high:
              result.add " or "

            elif idx > 0:
              result.add ", "

            result.add toStr(sub)

    else:
      raise newImplementKindError(tree)

func `$`*[T](w: WordTree[T]): string = toStr(w)

echo quote($~"1", $~"2").anyOf()
echo quote($~"1", $~"2", $~"3").anyOf()

for idx in 0 ..< 5:
  echo many(
    "Allowed items - ",
    "No items allowed",
    wordMap([0,2,3,4,5,6,7][0..idx]).quote().anyOf()
  ).sent()

echo expl($~"test", $~"missing")
echo quote($~"1", $~"2", $~"3").anyOf()
echo anyOf($~"test", $~"test1")
echo wantGot($~"at least tree", $~"none")
echo wantGot($~"file", desc("extension ", $~"ext"), $~"filename without any extension").sent()

echo par(
  $~~"no matching option",
  gotWant($~(1 + 2), anyOf($~[1,2,3,4])).sent()
)
