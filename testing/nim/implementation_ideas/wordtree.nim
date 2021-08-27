import hmisc/core/all

type
  WordKind* = enum
    wkText
    wkNewline
    wkPunctuation
    wkSpace
    wkMarkup

  Word*[Text, Attr] = object
    text*: Text
    attr*: Attr
    forceNl: bool
    isBreaking: bool
    kind*: WordKind

  WordTreeKind = enum
    wtStrong
    wtEmphasis
    wtUnderscore
    wtCode
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

func tree[T, A](
    kind: WordTreeKind, word: Word[T, A],
    subnodes: varargs[WordTree[T, A]]): WordTree[T, A] =
  WordTree[T, A](kind: kind, word: word, subnodes: @subnodes)


func word(s: string): TextTree = tree(wtWord, TextWord(text: s))

const noWord = TextWord()

func strong[T, A](words: varargs[WordTree[T, A]]): WordTree[T, A] = tree(wtStrong, noWord, words)
func anyOf[T, A](words: varargs[WordTree[T, A]]): WordTree[T, A] = tree(wtAnyOf, noWord, words)

func `[]`[T, A](tree: WordTree[T, A], idx: int): WordTree[T, A] = tree.subnodes[idx]

func `$`[T, A](tree: WordTree[T, A]): string =
  case tree.kind:
    of wtStrong:
      result.add "*"
      for sub in tree.subnodes:
        result.add $sub
      result.add "*"

    of wtWord:
      result.add $tree.word.text

    of wtAnyOf:
      case tree.subnodes.len:
        of 0:
          raise newArgumentError("Empty 'any of' list")

        of 1:
          return $tree.subnodes[0]

        of 2:
          result = &["either ", $tree[0], " or ", $tree[1]]

        else:
          for idx, sub in pairs(tree.subnodes):
            if idx == tree.subnodes.high:
              result.add " or "

            elif idx > 0:
              result.add ", "

            result.add $sub

    else:
      raise newImplementKindError(tree)


echo anyOf(word("test"), word("test1"))
