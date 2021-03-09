#!/usr/bin/env nim

var cb: proc() # not initialized, but is still considered
               # non-nil.
echo cb == nil
echo cb != nil
