proc impl*[T](arg: T): void =
  echo "Generic fallback implementation provided my library"

proc impl*[T](arg: seq[T]): void =
  echo "Less generic implementation for ", typeof(arg)
  for val in arg:
    impl(val)
