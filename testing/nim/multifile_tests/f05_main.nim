# `exported` proc uses `sets.difference` internally, which in turn
# depends on `sets.items`. Latter one is used implicty (e.g `for item
# in s1` and `mixin` annotation has no effect on it.)

import f05_lib_impl

exported(12)
