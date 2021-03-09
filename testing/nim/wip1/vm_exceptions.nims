type
  Ex = ref object of CatchableError

try:
  raise Ex(msg: "wer")
except:
  discard
  # discard cast[Ex](getCurrentException()) # <-- not supported
