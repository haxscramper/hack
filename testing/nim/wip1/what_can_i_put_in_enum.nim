type
  # string
  A = enum
    a = "eee"

  # float - XXX - does not work
  # B = enum
  #   b = 1.2

  # integer
  C = enum
    c = 12

  # char
  E = enum
    e = 'e'

  # tuple (int, string)
  F = enum
    f = (12, "EE")

  # # tuple (char, char) - XXX - does not work
  # G = enum
  #   g = ('2', 'e')

  # # tuple (string, string) - XXX - does not work
  # H = enum
  #   h = ("ee", "ee")
