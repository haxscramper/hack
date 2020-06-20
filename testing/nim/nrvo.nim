type
  Type = object
    f1: string

proc r1(): Type {.raises: [IoError].} = discard
proc r2(): Type {.raises: [].} = discard

let v1 = r1() # Gives `ObservableStores` warning because
              # `r1` can raise exception
let v2 = r2()
