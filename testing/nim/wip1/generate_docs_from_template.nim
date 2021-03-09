template makeDoc*(): untyped =
  ## DOcumentation for template
  proc hello*(): void =
    ## DOcumentation for hello proc
    echo 123


makeDoc()

hello()
