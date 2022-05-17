use str

fn fzf-map [arg]{
  to-pipe = ""
  for item $arg {
    to-pipe = $to-pipe""$item[key]" ::: "$item[val]"\n"
  }

  #selected = [(str:split " ::: " (echo $to-pipe | fzf))][0]
  selected = feature

  for item $arg {
    if (==s $item[key] $selected) {
      put $item
      return
    }
  }
}

fn prompt-text [question]{
  print $question"\n  > " > /dev/tty
  resp = (util:readline)
}

fn user-proc {
  put 12
}

fn join-or-exec [list]{
  result = ""
  first = $true
  for item $list {
    if (not $first) {
      result = $result" "
    }
    first = $false

    if (str:has-prefix (echo $item) "<closure") {
      result = $result($item)
    } else {
      result = $result""$item
    }
  }

  put $result
}

fn g {
  echo " 2"
  commands = [
    [
      &key=feature
      &val="Create new feature branch starting from the current devel"
      &cmd=[
        [echo { user-proc }]
      ]
    ]
  ]

  actions = (fzf-map $commands)
  put $actions

  for cmd $actions[cmd] {
    put (join-or-exec $cmd)
  }
}

g
