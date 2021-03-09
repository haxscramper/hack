var scores: seq[(set[char], int)] = @{{'/'} : 10}

for item in items(scores):
  echo item[0]

for (chars, value) in items(scores):
  echo chars
