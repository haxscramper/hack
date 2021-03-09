#!/usr/bin/env python
import dis

def func(b, c):
    for i in [1, 2, 3]:
        skip
      # print(i + b * c)


# dis.dis(func)

dis.dis("for i in [1, 2]: skip")
print("-----------------------")
dis.dis("""
for i in [1, 2, 3]:
  print(i + 2)
  if ((i + 2) == 4):
    print("Found 2")

  else:
    print(i)
""")
