import hmisc/core/all

/// "before file 4 imports file 2":
  echo "4 -> 2"

import ./file2

// "after file 4 imported file 2"


/// "Import file 3":
  import ./file3
