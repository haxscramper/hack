import typetraits

block:
  proc adds(a1: static[int], a2: static[int]): static[int] =
    echo "sdf"
    return a1 + a2

  type
    U[T: static[int]] = object


  proc multiply[N1, N2: static[int]](
    a1: U[N1], a2: U[N2]): U[adds(N1, N2)] =
      discard

  var u1: U[1]
  var u2: U[2]

  static:
    echo typeof(multiply(u1, u2))


block:
  type
    U[S: static[string]] = object

  proc concat(a1, a2: static[string]): static[string] =
    echo "concat"
    return a1 & a2

  static:
    echo typeof static[string]("test")
    echo typeof concat("12", "12")

block:
  # Error: cannot infer the value of the static param 'N1'
  proc concat[N1, N2: static[int]](
    a1: static[array[N1, string]], a2: static[array[N2, string]]
             ): static[array[N1 + N2, string]] =
    echo "concat"
    return [a1[0], a2[0]]

  static:
    echo typeof concat[
      1, 1,
      static[array[1, string]],
      static[array[1, string]]
    ](["sdf"], ["sdf"])

  type
    U[N: static[int], T: static[array[N, string]]] = object

  proc multiply[N1, N2: static[int]](
    # Args
    a1: U[N1, static[array[N1, string]]],
    a2: U[N2, static[array[N2, string]]]
  ):
    # Return type
    U[N1 + N2, static[array[N1 + N2, string]]] =

    discard
