import z3

for ver in [0, 1]:
  z3:
    let main = Int("main")
    let A = Int("A")
    let B = Int("B")
    let C = Int("C")

    let s = Solver()
    s.assert (main <-> A)
    s.assert (
      ((A == 0) and (B >= 1) and (C > 0)) or
      ((A == 1) and (B >= 1) and (C > 1))
    )

    s.assert A >= ver

    if s.check() == Z3_L_TRUE:
      echo "--------------"
      echo ver
      echo s.getModel()
