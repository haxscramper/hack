import nanim


proc daily094Scene(): Scene =
  let scene = newScene()
  scene.randomize()

  var colors = colorsFromCoolors("https://coolors.co/cfdbd5-e8eddf-f5cb5c-242423-333533")
  colors.shuffle()

  let bg = colors[0]

  scene.setBackgroundColor(bg)
  colors.del(0)


  for i in 0..60:
    let d = newDot(rand(6.0..190.0))
    d.fill(colors.sample())

    d.moveTo(rand(0.0..1000.0), rand(0.0..1000.0))
    scene.add(d)

    scene.onTrack i:
      scene.sleep(rand(0.0..4000.0))
      scene.play(d.scaleTo(1.5), d.fadeOut())
      scene.play(d.scaleTo(0.0).with(duration=0.0))
      scene.play(d.scaleTo(1.0), d.fadeIn())

  return scene


when isMainModule:
  render(daily094Scene)
