import macros

dumpTree:
  type
    U* {.derive(
      Eq, # Automatically derive `==` pattern
      Hash, # Implement `hash` function
      Validation, # Recognize `check` in pragmas - generate validators
      Constructor, # Provide constructor proc,
      Qt(Widget, Metatype),
      ExportCpp(Override), # Export as CPP class
      Methods
    ).} = object
      f1* {.[ # Annotated as exported, but due to `getset` annotation
              # and validator it will not be exported directly.
              # Instead `func check` and `func check=` will be
              # generated.
        check(it < 10), # Field value should be less than 10
        getset
      ].}: int = 5

      f2* {.check(it.starsWith("H")).}: string
      # This field is not exported and external access is disabled for
      # it - setters/getters are not expored.
      internal {.[
        passthrough((`[]=`, var U, float, int), check(a1 < 1.0))
      ].}: Table[float, int]

  method inc*(this: U) {.base.}
  method paintEvent(this: U, event: QPaintEvent) {.cppOverride.}
