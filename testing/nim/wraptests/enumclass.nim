{.emit: """
enum class Hello {
  hHello
};
""".}

type
  Hello {.importcpp: "Hello".} = enum
    hHello {.importcpp: "Hello::hHello".}
