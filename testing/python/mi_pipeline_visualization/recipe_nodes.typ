#set text(font: "Iosevka Term")

#let debug_text(text_in, color) = {
  place(
    top + left,
    dx: 0pt,
    dy: 0pt,
    text(
      size: 2pt,
      fill: color,
      weight: "bold",
      text_in,
    ),
  )
}

#let debug_point(x, y, color) = {
  place(
    top + left,
    dx: 0pt,
    dy: 0pt,
    circle(radius: 0.5pt, fill: color),
  )
  debug_text(repr((x, y)), color)
}

#let debug_abs_point(x, y, color) = {
  place(
    dx: x * 1pt,
    dy: y * 1pt,
    debug_point(x, y, color),
  )
}

#let draw_arrow(direction, width, height) = {
  let width = width * 1pt
  let height = height * 1pt
  if direction == "in" {
    place(
      center,
      polygon(
        fill: black,
        (0pt, 0pt),
        (width * 0.6, height * 0.2),
        (0pt, width * 0.4),
      ),
    )
  } else if direction == "out" {
    place(
      center,
      polygon(
        fill: black,
        (0pt, 0pt),
        (width * 0.6, height * 0.2),
        (0pt, width * 0.4),
      ),
    )
  }
}

#let draw_port(port) = {
  let port_x = port.at("x", default: 0)
  let port_y = port.at("y", default: 0)
  let direction = port
    .at("extra", default: (:))
    .at("data", default: (:))
    .at("data", default: (:))
    .at("direction", default: "")
  let port_side = port
    .at("properties", default: (:))
    .at("port_side", default: "")

  let port_width = 8
  let port_height = 8
  let rect_x = 0
  let rect_y = 0

  // If the port has explicit width and height it is going to be properly offset
  // relative to the parent node. But if the port originally had no dimensions,
  // the ELK layout treats it as a zero-sized point.
  if "width" in port and "height" in port {
    port_width = port.at("width")
    port_height = port.at("height")
    rect_x = port_x - port_width / 2
    rect_y = port_y - port_height / 2
  } else {
    // Determine the placement of the port rectangle based on the port side
    if port_side == "WEST" {
      rect_x = port_x - port_width
      rect_y = port_y - port_height / 2
    } else if port_side == "EAST" {
      rect_x = port_x
      rect_y = port_y - port_height / 2
    } else if port_side == "NORTH" {
      rect_x = port_x - port_width / 2
      rect_y = port_y - port_height
    } else if port_side == "SOUTH" {
      rect_x = port_x - port_width / 2
      rect_y = port_y
    } else {
      // Default: center the rectangle
      rect_x = port_x - port_width / 2
      rect_y = port_y - port_height / 2
    }
  }

  // Draw the port rectangle
  place(
    dx: rect_x * 1pt - 1pt,
    dy: rect_y * 1pt - 1pt,
    stack(
      rect(
        width: port_width * 1pt,
        height: port_height * 1pt,
        stroke: black + 1pt,
        fill: white,
        // draw_arrow(direction, port_width, port_height),
      ),
      // Draw direction arrow inside the rectangle
    ),
  )
}

#let draw_node_base(node, fill_color) = {
  rect(
    width: 100%,
    height: 100%,
    stroke: black + 2pt,
    fill: fill_color,
    radius: 5pt,
    // Place ports inside this rectangle
    if "ports" in node {
      for port in node.ports {
        draw_port(port)
      }
    },
  )
}

#let draw_node_image(node) = {
  if (
    "extra" in node
      and "data" in node.extra
      and "data" in node.extra.data
      and "image" in node.extra.data.data
  ) {
    let image_path = node.extra.data.data.image
    place(
      center + horizon,
      image(image_path, width: 32pt, height: 32pt),
    )
  }
}

#let box_at(..args) = {
  place(
    dx: args.at("x") * 1pt,
    dy: args.at("y") * 1pt,
    box(
      inset: args.at("inset", default: 0pt),
      width: args.at("width") * 1pt,
      height: args.at("height") * 1pt,
      place(
        args.at("body"),
      ),
    ),
  )
}

#let node_box(node, body, x_offset, y_offset) = {
  place(
    dx: (node.x + x_offset) * 1pt,
    dy: (node.y + y_offset) * 1pt,
    box(
      width: node.width * 1pt,
      height: node.height * 1pt,
      body,
    ),
  )
}

#let fluid_node(node) = {
  node_box(node, draw_node_base(node, blue.lighten(80%)), 0, 0)
  node_box(node, draw_node_image(node), 0, 0)
}

#let item_node(node) = {
  node_box(node, draw_node_base(node, orange.lighten(80%)), 0, 0)
  node_box(node, draw_node_image(node), 0, 0)
}

#let format_amount(content) = {
  if "amount" in content and content.amount != none {
    if (content.node_kind == "fluid") {
      if 1000 < content.amount {
        return str(content.amount / 1000) + "B"
      } else {
        return str(content.amount) + "mB"
      }
    } else {
      return str(content.amount)
    }
  } else {
    return ""
  }
}



#let draw_slot(x, y, slot_size, slot_type, content) = {
  let fill_color = if slot_type == "item" {
    rgb("#8B8B8B").lighten(80%)
  } else {
    blue.lighten(90%)
  }

  let stroke_color = if slot_type == "item" {
    gray
  } else {
    blue
  }

  let box_args = (
    x: x,
    y: y,
    width: slot_size,
    height: slot_size,
    inset: 1pt,
  )

  box_at(
    ..box_args,
    body: rect(
      stroke: 0.5pt + stroke_color,
      fill: fill_color,
      width: 100%,
      height: 100%,
      radius: 2pt,
    ),
  )

  if content != none {
    if "image" in content {
      box_at(..box_args, inset: 3pt, body: image(
        content.image,
        width: 100%,
        height: 100%,
      ))
    } else {
      box_at(..box_args, body: text(
        hyphenate: true,
        size: 3pt,
        content.id,
      ))
    }
    box_at(..box_args, inset: 2pt, body: box(width: 100%, height: 100%, place(
      bottom + right,
      text(
        size: 5pt,
        format_amount(content),
      ),
    )))
  }
}

#let draw_slot_grid(slot_position, slot_size, slot_type, contents) = {
  if slot_position == none {
    return
  }

  for row in range(slot_position.rows) {
    for col in range(slot_position.cols) {
      let idx = row * slot_position.cols + col
      let x = slot_position.x + col * slot_size
      let y = slot_position.y + row * slot_size
      let content = if idx < contents.len() { contents.at(idx) } else { none }
      draw_slot(x, y, slot_size, slot_type, content)
    }
  }
}


#let draw_recipe_gui(data) = {
  let machine = data.machine
  let slot_size = 18

  // Draw item input slots
  draw_slot_grid(
    machine.item_slots.positions.at(0, default: none),
    slot_size,
    "item",
    data.data.item_inputs,
  )

  // Draw item output slots
  draw_slot_grid(
    machine.item_slots.positions.at(1, default: none),
    slot_size,
    "item",
    data.data.item_outputs,
  )

  // Draw fluid input slots
  draw_slot_grid(
    machine.fluid_slots.positions.at(0, default: none),
    slot_size,
    "fluid",
    data.data.fluid_inputs,
  )

  // Draw fluid output slots
  draw_slot_grid(
    machine.fluid_slots.positions.at(1, default: none),
    slot_size,
    "fluid",
    data.data.fluid_outputs,
  )

  // Draw progress bar
  place(
    dx: machine.progress_bar.x * 1pt,
    dy: machine.progress_bar.y * 1pt,
    rect(
      stroke: 0.5pt + gray,
      fill: green.lighten(80%),
      width: 24pt,
      height: 18pt,
    ),
  )

  // // Draw energy bar
  // place(
  //   dx: machine.energy_bar.x * 1pt,
  //   dy: machine.energy_bar.y * 1pt,
  //   rect(
  //     stroke: 0.5pt + gray,
  //     fill: yellow.lighten(80%),
  //     width: 12pt,
  //     height: 18pt,
  //   )
  // )

  // // Draw efficiency bar
  // place(
  //   dx: machine.efficiency_bar.x * 1pt,
  //   dy: machine.efficiency_bar.y * 1pt,
  //   rect(
  //     stroke: 0.5pt + gray,
  //     fill: orange.lighten(80%),
  //     width: 24pt,
  //     height: 12pt,
  //   )
  // )

  // // Draw EU and duration text
  // place(
  //   dx: machine.progress_bar.x * 1pt,
  //   dy: (machine.progress_bar.y - 12) * 1pt,
  //   text(size: 8pt, str(data.data.eu) + " EU/t")
  // )

  // place(
  //   dx: machine.progress_bar.x * 1pt,
  //   dy: (machine.progress_bar.y + 20) * 1pt,
  //   text(size: 8pt, str(data.data.duration) + " ticks")
  // )
}

#let format_content(content) = {
  let name = content.id.split(":").at(1)
  if "amount" in content and content.amount != none {
    return name + " (" + format_amount(content) + ")"
  } else {
    return name
  }
}

#let format_content_list(items) = {
  if items.len() == 1 {
    format_content(items.at(0))
  } else {
    list(..items.map(item => format_content(item)))
  }
}

#let get_recipe_section(col2, items, label) = {
  let formatted = format_content_list(items)
  return ([*#label:*], [#formatted])
}


#let draw_recipe_info(data) = {
  let recipe_data = data.data
  let machine = data.machine

  let col1 = ()
  let col2 = ()

  col1.push(([*Machine:*], [#machine.english_name]))
  if "eu" in recipe_data {
    col1.push(([*Energy:*], [#recipe_data.eu EU/t]))
  }

  if "duration" in recipe_data {
    col1.push(([*Duration:*], [#recipe_data.duration ticks]))
  }

  if 0 < recipe_data.item_inputs.len() {
    col2.push(get_recipe_section(col2, recipe_data.item_inputs, "Input Items"))
  }
  if 0 < recipe_data.item_outputs.len() {
    col2.push(get_recipe_section(
      col2,
      recipe_data.item_outputs,
      "Output Items",
    ))
  }
  if 0 < recipe_data.fluid_inputs.len() {
    col2.push(get_recipe_section(
      col2,
      recipe_data.fluid_inputs,
      "Input Fluids",
    ))
  }
  if 0 < recipe_data.fluid_outputs.len() {
    col2.push(get_recipe_section(
      col2,
      recipe_data.fluid_outputs,
      "Output Fluids",
    ))
  }

  let max_rows = calc.max(col1.len(), col2.len())
  while col1.len() < max_rows { col1.push(([], [])) }
  while col2.len() < max_rows { col2.push(([], [])) }

  let table_rows = ()
  for i in range(max_rows) {
    table_rows.push(col1.at(i).at(0))
    table_rows.push(col1.at(i).at(1))
    table_rows.push(col2.at(i).at(0))
    table_rows.push(col2.at(i).at(1))
  }

  text(
    size: 5pt,
    pad(
      4pt,
      table(
        columns: (1fr, auto, 1fr, auto),
        stroke: none,
        inset: 2pt,
        ..table_rows
      ),
    ),
  )
}

#let recipe_node(node) = {
  node_box(node, draw_node_base(node, green.lighten(80%)), 0, 0)
  node_box(node, draw_recipe_info(node.extra.data), 0, 0)
  node_box(node, draw_recipe_gui(node.extra.data), 0, 20)
}

#let edge(edge_data) = {
  if "sections" in edge_data {
    for section in edge_data.sections {
      let start = section.at("startPoint")
      let end = section.at("endPoint")

      let curve_elements = ()

      if "bendPoints" in section {
        let points = (start,) + section.at("bendPoints") + (end,)

        curve_elements.push(curve.line((
          points.at(0).at("x") * 1pt,
          points.at(0).at("y") * 1pt,
        )))

        for i in range(1, points.len()) {
          let point = points.at(i)
          curve_elements.push(curve.line((
            point.at("x") * 1pt,
            point.at("y") * 1pt,
          )))
        }
      } else {
        curve_elements.push(curve.line((
          start.at("x") * 1pt,
          start.at("y") * 1pt,
        )))
        curve_elements.push(curve.line((end.at("x") * 1pt, end.at("y") * 1pt)))
      }

      // debug_abs_point(start.x, start.y, blue)
      // debug_abs_point(end.x, end.y, blue)

      place(
        curve(
          stroke: black + 1pt,
          curve.move((start.x * 1pt, start.y * 1pt)),
          ..curve_elements,
        ),
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
            stroke: gray + 0.5pt,
          ),
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
            stroke: gray + 0.5pt,
          ),
        )
      }
    },
  )

  // Minor grid (2pt spacing)
  place(
    top + left,
    dx: 0pt,
    dy: 0pt,
    {
      // Vertical lines
      for x in range(0, int(horizontal_size), step: 2) {
        if calc.rem(x, 10) != 0 {
          // Skip major grid lines
          place(
            dx: x * 1pt,
            dy: 0pt,
            line(
              start: (0pt, 0pt),
              end: (0pt, horizontal_size * 1pt),
              stroke: gray.lighten(50%) + 0.25pt,
            ),
          )
        }
      }

      // Horizontal lines
      for y in range(0, int(vertical_size), step: 2) {
        if calc.rem(y, 10) != 0 {
          // Skip major grid lines
          place(
            dx: 0pt,
            dy: y * 1pt,
            line(
              start: (0pt, 0pt),
              end: (vertical_size * 1pt, 0pt),
              stroke: gray.lighten(50%) + 0.25pt,
            ),
          )
        }
      }
    },
  )
}
