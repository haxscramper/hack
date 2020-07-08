type
  GenEx[T] = ref object of CatchableError
    info: T

block:
  try:
    raise GenEx[int](info: 12)
  except GenEx[int]:
    echo "caught integer exception"

block:
  try:
    try:
      raise GenEx[string](info: "Hello world")
    except GenEx[int]:
      echo "Caught integer exception, but expected string"
  except GenEx[string]:
    echo "Caught string exception"
    let e = getCurrentException()
    let eCast = cast[GenEx[string]](e)
    echo  "Exception contains: ", eCast.info
