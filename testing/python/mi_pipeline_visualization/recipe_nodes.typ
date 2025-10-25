
#let draw_port(x: 0, y: 0, direction: "", radius: 3pt) = {
  // Draw port circle centered at x, y
  place(
    dx: x * 1pt - radius,
    dy: y * 1pt - radius,
    circle(
      radius: radius,
      stroke: black + 1pt,
      fill: white
    )
  )
  
  // Draw direction arrow
  if direction == "in" {
    place(
      dx: (x - 8) * 1pt,
      dy: (y - 2) * 1pt,
      polygon(
        fill: black,
        (0pt, 0pt),
        (6pt, 2pt),
        (0pt, 4pt)
      )
    )
  } else if direction == "out" {
    place(
      dx: (x + 8) * 1pt,
      dy: (y - 2) * 1pt,
      polygon(
        fill: black,
        (0pt, 0pt),
        (6pt, 2pt),
        (0pt, 4pt)
      )
    )
  }
}

#let draw_node_base(node, fill_color) = {
  let x = node.x
  let y = node.y
  let width = node.width
  let height = node.height
  
  place(
    dx: x * 1pt,
    dy: y * 1pt,
    rect(
      width: width * 1pt,
      height: height * 1pt,
      stroke: black + 1pt,
      fill: fill_color
    )
  )
  
  if "ports" in node {
    for port in node.ports {
      let port_x = x + port.at("x", default: 0)
      let port_y = y + port.at("y", default: 0)
      let direction = port.at("extra", default: (:)).at("data", default: (:)).at("data", default: (:)).at("direction", default: "")
      
      draw_port(x: port_x, y: port_y, direction: direction)
    }
  }
}

#let fluid_node(node) = {
  draw_node_base(node, blue.lighten(80%))
}

#let item_node(node) = {
  draw_node_base(node, orange.lighten(80%))
}

#let recipe_node(node) = {
  draw_node_base(node, green.lighten(80%))
}

#let edge(edge_data) = {
  if "sections" in edge_data {
    for section in edge_data.sections {
      let start = section.at("startPoint")
      let end = section.at("endPoint")
      
      let curve_elements = ()
      
      if "bendPoints" in section {
        let points = (start,) + section.at("bendPoints") + (end,)
        
        curve_elements.push(curve.line((points.at(0).at("x") * 1pt, points.at(0).at("y") * 1pt)))
        
        for i in range(1, points.len()) {
          let point = points.at(i)
          curve_elements.push(curve.line((point.at("x") * 1pt, point.at("y") * 1pt)))
        }
      } else {
        curve_elements.push(curve.line((start.at("x") * 1pt, start.at("y") * 1pt)))
        curve_elements.push(curve.line((end.at("x") * 1pt, end.at("y") * 1pt)))
      }

      
      place(
        curve(
          stroke: black + 1pt,
          curve.move((start.x * 1pt, start.y * 1pt)),
          ..curve_elements
        )
      )
    }
  }
}
