fn all-of {|list pred|
  for item $list {
    if (not ($pred $item)) { put $false ; return }
  }
  put $true
}

fn any-of {|list pred|
  for item $list {
    if ($pred $item) { put $true; return }
  }
  put $false
}

fn none-of {|list pred|
  for item $list {
    if ($pred $item) { put $false ; return }
  }
  put $true
}

echo (all-of [1 2 3] {|x| put (< $x 10) })
echo (all-of [1 "2" 3] {|x| put (==s "string" (kind-of $x)) })
echo (all-of [1 "2" [3]] {|x| put (==s "string" (kind-of $x)) })
