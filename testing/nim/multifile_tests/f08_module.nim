type
  H* = object


proc `=destroy`(h: var H) = echo "Destructor for H called"
