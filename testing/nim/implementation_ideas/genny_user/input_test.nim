import hmisc/core/all

type
  Obj = object
    sub: seq[int]

iterator items*(o: Obj): int =
  // "Items iterator start"
  for i in o.sub:
    yield i

  // "Items iterator end"

proc main() =
  block:
    // "iterator over items"
    let closeItems = iterator(): int =
                       // "Body of the closure iterator"
                       for item in Obj(sub: @[1,2,3,4]):
                         // "Yield item here"
                         yield item
                         // "Yield finished"

                       // "Closure iterator finished"

    // "Call echo"
    echo closeItems()

  block:
    // "Single iterator block"
    let closeSingle = iterator(): int =
                        // "single item yield start"
                        yield 12
                        // "single item yield end"

    // "Calling closure single"
    echo closeSingle()
    // "Called single closure"

  block:
    // "while iterator block"
    let closeWhile = iterator(): int =
                       var it = 0
                       cblock "While loop":
                         while it < 5:
                           cblock "While body":
                             yield it

    cblock "Call while closure iterator":
      echo closeWhile()

  cblock "integer operations":
    var s = @[2,3,4,54]
    cblock "Sequence len":
      let l {.noinit.} = s.len

    cblock "Add item to sequence":
      s.add 12

    cblock "Get item from the sequence":
      let i = s[0]

  cblock "Object sequence operations":
    var s: seq[Obj]
    cblock "Add items to sequence":
      s.add Obj()

    cblock "Get item from the sequence":
      let i = s[0]



main()
