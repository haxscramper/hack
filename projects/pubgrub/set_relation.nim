type
  SetRelation {.pure.} = enum
    subset ## The second set contains all elements of the first, as well as
           ## possibly more.

    disjoint ## Neither set contains any elements of the other.

    overlapping ## The sets have elements in common, but the first is not a
    ## superset of the second.
    ##
    ## This is also used when the first set is a superset of the first, but
    ## in practice we don't need to distinguish that from overlapping sets.
