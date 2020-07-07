import strformat

let areas = @[
  "game", "web", "tools", "science", "systems", "embedded", "drivers", "GUI", "mobile"
]

for area in areas:
  echo &"Hello {area} developers!"
