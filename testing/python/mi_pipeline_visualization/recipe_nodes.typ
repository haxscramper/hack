
#let draw_port(port) = {
  let port_x = port.at("x", default: 0)
  let port_y = port.at("y", default: 0)
  let port_width = port.at("width", default: 6)
  let port_height = port.at("height", default: 6)
  let direction = port.at("extra", default: (:)).at("data", default: (:)).at("data", default: (:)).at("direction", default: "")

  


  place(
    dx: (port_x) * 1pt,
    dy: (port_y) * 1pt,
    stack(
      rect(
        width: port_width * 1pt,
        height: port_height * 1pt,
        stroke: black + 1pt,
        fill: white,
      ),
      place(
        text(
          size: 2pt,
          fill: red,
          weight: "bold",
          repr(port)
        )
      )
    )
  )
  
  // // Direction arrows
  // if direction == "in" {
  //   place(
  //     dx: (port_x - 8) * 1pt,
  //     dy: (port_y - 2) * 1pt,
  //     polygon(
  //       fill: black,
  //       (0pt, 0pt),
  //       (6pt, 2pt),
  //       (0pt, 4pt)
  //     )
  //   )
  // } else if direction == "out" {
  //   place(
  //     dx: (port_x + 8) * 1pt,
  //     dy: (port_y - 2) * 1pt,
  //     polygon(
  //       fill: black,
  //       (0pt, 0pt),
  //       (6pt, 2pt),
  //       (0pt, 4pt)
  //     )
  //   )
  // }
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
      fill: fill_color,
      // Place ports inside this rectangle
      if "ports" in node {
        for port in node.ports {
          draw_port(port)
        }
      }
    )
  )
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


#let draw_grid(horizontal_size, vertical_size) = {
  let page_width = 100%
  let page_height = 100%
  
  // Major grid (10pt spacing)
  place(
    top + left,
    dx: 0pt,
    dy: 0pt,
    {
      // Vertical lines
      for x in range(0, int(horizontal_size), step: 10) {
        place(
          dx: x * 1pt,
          dy: 0pt,
          line(
            start: (0pt, 0pt),
            end: (0pt, horizontal_size * 1pt),
            stroke: gray + 0.5pt
          )
        )
      }
      
      // Horizontal lines
      for y in range(0, int(vertical_size), step: 10) {
        place(
          dx: 0pt,
          dy: y * 1pt,
          line(
            start: (0pt, 0pt),
            end: (vertical_size * 1pt, 0pt),
            stroke: gray + 0.5pt
          )
        )
      }
    }
  )
  
  // Minor grid (2pt spacing)
  place(
    top + left,
    dx: 0pt,
    dy: 0pt,
    {
      // Vertical lines
      for x in range(0, int(horizontal_size), step: 2) {
        if calc.rem(x, 10) != 0 { // Skip major grid lines
          place(
            dx: x * 1pt,
            dy: 0pt,
            line(
              start: (0pt, 0pt),
              end: (0pt, horizontal_size * 1pt),
              stroke: gray.lighten(50%) + 0.25pt
            )
          )
        }
      }
      
      // Horizontal lines
      for y in range(0, int(vertical_size), step: 2) {
        if calc.rem(y, 10) != 0 { // Skip major grid lines
          place(
            dx: 0pt,
            dy: y * 1pt,
            line(
              start: (0pt, 0pt),
              end: (vertical_size * 1pt, 0pt),
              stroke: gray.lighten(50%) + 0.25pt
            )
          )
        }
      }
    }
  )
}
