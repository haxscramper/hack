import f00_library

proc procedure*(a: int, b: float): string =
  echo "wrapper procedure"

echo procedure(12, 2.3)
