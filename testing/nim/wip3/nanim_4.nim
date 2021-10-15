import pkg/[nanim, nanovg]
import pkg/nanim/entities/rectangle

proc testScene(): Scene =
  let scene = newScene()
  let bg = rgb(128, 128, 230)
  let fg = rgb(128, 230, 128)

  scene.setBackgroundColor(bg)

  var rects: seq[Rectangle]
  for i in 0 .. 4:
    let rect = newSquare()
    rect.fill(fg)
    scene.add(rect)
    rects.add(rect)

  scene.wait(500)
  scene.showAllEntities()

  for idx, rect in rects:
    scene.play(rect.move(100 + idx * 200.0, 500))

  scene.wait(500)

  return scene


when isMainModule:
  render(testScene)
