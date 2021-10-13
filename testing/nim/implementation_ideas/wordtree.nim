import hmisc/core/all

type
  WordKind* = enum
    wkText
    wkNewline
    wkPunctuation
    wkSpace
    wkMarkup
    wkVerbatim

  Word*[Text, Attr] = object
    text*: Text
    attr*: Attr
    forceNl: bool
    isBreaking: bool
    kind*: WordKind

  WordTreeKind = enum
    wtConcat
    wtSentence

    wtDescItem
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


  WordTree*[Text, Attr] = ref object
    kind: WordTreeKind
    subnodes: seq[WordTree[Text, Attr]]
    word: Word[Text, Attr]

  TextWord = Word[string, char]
  TextTree = WordTree[string, char]

func len*[T, A](t: WordTree[T, A]): int =
  t.subnodes.len

func tree*[T, A](
    kind: WordTreeKind,
    subnodes: varargs[WordTree[T, A]]): WordTree[T, A] =
  WordTree[T, A](kind: kind, subnodes: @subnodes)

func tree*[T, A](
    kind: WordTreeKind, word: Word[T, A],
    subnodes: varargs[WordTree[T, A]]): WordTree[T, A] =
  WordTree[T, A](kind: kind, word: word, subnodes: @subnodes)



func initWord*(word: var string, s: string) = word = s

func word*[T, A](s: string): WordTree[T, A] =
  var text: T
  initWord(text, s)
  tree(wtWord, Word[T, A](text: text))

func textWord*(s: string): TextTree = word[string, char](s)
proc `$~`*[T](arg: T): TextTree = textWord($arg)

func wordMap*[O, T, A](
    obj: openarray[O], fmap: proc(obj: O): WordTree[T, A]): seq[WordTree[T, A]] =
  for item in obj:
    result.add fmap(item)

func wordMap*[O](obj: openarray[O]): seq[TextTree] =
  for item in obj:
    result.add textWord($item)

func strong*[T, A](words: varargs[WordTree[T, A]]): WordTree[T, A] = tree(wtStrong, @words)
func anyOf*[T, A](words: varargs[WordTree[T, A]]): WordTree[T, A] = tree(wtAnyOf, @words)
func wantGot*[T, A](want, got: WordTree[T, A]): WordTree[T, A] =
  tree(wtConcat, word[T, A]("expected "),
       want, word[T, A](", but got "), got)

func wantGot*[T, A](thing, want, got: WordTree[T, A]): WordTree[T, A] =
  tree(wtConcat, thing, word[T, A](" has "),
       got, word[T, A](", but expected "), want)

func desc*[T, A](desc: string, item: WordTree[T, A]): WordTree[T, A] =
  tree(wtDescItem, word[T, A](desc), item)

func sent*[T, A](words: varargs[WordTree[T, A]]): WordTree[T, A] =
  tree(wtSentence, @words)

func many*[T, A](onMany, onOne: string, tree: WordTree[T, A]): WordTree[T, A] =
  if len(tree) == 0:
    result = word[T, A](onOne)

  else:
    result = tree(wtConcat, word[T, A](onMany), tree)



func quote*[T, A](words: varargs[WordTree[T, A]]): seq[WordTree[T, A]] =
  for word in words:
    result.add tree(wtQuoted, word)

func `[]`*[T, A](tree: WordTree[T, A], idx: int): WordTree[T, A] = tree.subnodes[idx]

func toStr*[T, A](tree: WordTree[T, A]): string =
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

    of wtWord:
      result.add $tree.word.text

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

echo quote($~"1", $~"2").anyOf().toStr()
echo quote($~"1", $~"2", $~"3").anyOf().toStr()

for idx in 0 ..< 5:
  echo many(
    "Allowed items - ",
    "No items allowed",
    wordMap([0,2,3,4,5,6,7][0..idx]).quote().anyOf()
  ).sent().toStr()

echo quote($~"1", $~"2", $~"3").anyOf().toStr()
echo anyOf($~"test", $~"test1").toStr()
echo wantGot($~"at least tree", $~"none").toStr()
echo wantGot($~"file", desc("extension ", $~"ext"), $~"filename without any extension").sent().toStr()
