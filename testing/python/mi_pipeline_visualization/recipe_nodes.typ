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
      
      place(
        dx: port_x * 1pt,
        dy: port_y * 1pt,
        circle(
          radius: 3pt,
          stroke: black + 1pt,
          fill: white
        )
      )
      
      if direction == "in" {
        place(
          dx: (port_x - 8) * 1pt,
          dy: port_y * 1pt,
          polygon(
            fill: black,
            (0pt, -2pt),
            (6pt, 0pt),
            (0pt, 2pt)
          )
        )
      } else if direction == "out" {
        place(
          dx: (port_x + 8) * 1pt,
          dy: port_y * 1pt,
          polygon(
            fill: black,
            (0pt, -2pt),
            (6pt, 0pt),
            (0pt, 2pt)
          )
        )
      }
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
      repr(section)
      // let start = section.at("startPoint")
      // let end = section.at("endPoint")
      
      // place(
      //   line(
      //     start: (start.at("x") * 1pt, start.at("y") * 1pt),
      //     end: (end.at("x") * 1pt, end.at("y") * 1pt),
      //     stroke: black + 1pt
      //   )
      // )
      
      // if "bendPoints" in section {
      //   let points = (start,) + section.at("bendPoints") + (end,)
      //   for i in range(points.len() - 1) {
      //     let p1 = points.at(i)
      //     let p2 = points.at(i + 1)
      //     place(
      //       line(
      //         start: (p1.at("x") * 1pt, p1.at("y") * 1pt),
      //         end: (p2.at("x") * 1pt, p2.at("y") * 1pt),
      //         stroke: black + 1pt
      //       )
      //     )
      //   }
      // }
    }
  }
}
