type
  En1 = enum e1First, e1FirstCopy1, e1FirstCopy2, e1Second
  En2 = enum e2First, e2Second
  En3 = enum e3First, e3Second

  Obj = object
    case k1: En1
      of e1First, e1FirstCopy:
         case k2: En2
           of e2First:
             f12first: string

           of e2Second:
             f12second: string

      of e1Second:
        case k3: En3
          of e3First:
            f13first: string

          of e3Second:
            f13second: string


if false:
  # Error: unhandled exception: assignment to discriminant changes object
  # branch
  var obj: Obj
  obj.k1 = e1Second

proc newO(k1: En1, k2: En2) =
  let a = Obj(k1: k1, k2: k2)

if true:
  let kind = e1First
  newO(e1First, e2First)
  let obj =
    case kind:
      of e1First:
        Obj(k1: e1First, k2: e2First)

      of e1FirstCopy:
        Obj(k1: e1FirstCopy, k2: e2First)

      else:
        Obj()
