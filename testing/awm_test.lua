for _, c in ipairs(client.get()) do
  if c.class == "Chromium" then
    local geo = c:geometry()
    debug_notify(dump(geo))
    geo.width = 1200
    geo.height = 900
    c.floating = true
    c:geometry(geo)
  end
end
