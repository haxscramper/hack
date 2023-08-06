function defaultStyles() {
  return ({
    nodeShapeStroke : "#444488",
    nodeShapeFill : "#ddddff",
    nodeLabelStroke : "none",
    nodeLabelFill : "#000",
    nodeLabelFontFamily : "sans-serif",
    nodeLabelFontSize : "12px",
    edgeShapeStroke : "#222244",
    edgeShapeFill : "none",
    portShapeStroke : "#444488",
    portShapeFill : "#6666cc",
  })
};

d3.selection.prototype.appendComment = function(comment) {
  return this.each(
      function() { this.appendChild(document.createComment(comment)); });
};

function toSvgPaths(edge) {
  return edge.sections.map(section => {
    const data =
        [ section.startPoint, ...(section.bendPoints || []), section.endPoint ];
    return d3.line().x(d => d.x).y(d => d.y)(data);
  });
}

function renderGraph(svg, root, scaleFactor = 1.0) {
  const styles = defaultStyles();
  svg.attr("width", root.data.width).attr("height", root.data.height);

  // Define edge arrow marker
  const defs = svg.append("defs");
  defs.append("marker")
      .attr("id", "edgeShapeMarker")
      .attr("markerWidth", 10)
      .attr("markerHeight", 10)
      .attr("refX", 6)
      .attr("refY", 3)
      .attr("orient", "auto")
      .attr("markerUnits", "strokeWidth")
      .append("path")
      .attr("d", "M0,0 L0,6 L6,3 z")
      .attr("fill", styles.edgeShapeStroke);

  function drawEdge(parent, edge) {
    const edgePaths = toSvgPaths(edge);

    edgePaths.forEach(edgePathString => {
      parent.append("path")
          .attr("d", edgePathString)
          .attr("stroke", styles.edgeShapeStroke)
          .attr("fill", styles.edgeShapeFill)
          .attr("marker-end", "url(#edgeShapeMarker)");
    });
  }

  function drawNode(parent, node) {
    // Create a group for each node
    const g = parent.append("g").attr(
        "transform", `translate(${node.data.x}, ${node.data.y})`);
    g.appendComment("Node");

    // Add a rectangle for the node
    g.append("rect")
        .attr("x", node.data.x)
        .attr("y", node.data.y)
        .attr("width", node.data.width)
        .attr("height", node.data.height)
        .attr("stroke", styles.nodeShapeStroke)
        .attr("fill", styles.nodeShapeFill);

    // Add labels
    if (node.data.labels) {
      node.data.labels.forEach((label) => {
        g.append("text")
            .attr("x", node.data.x + label.x)
            .attr("y", node.data.y + label.y)
            .attr("stroke", styles.nodeLabelStroke)
            .attr("fill", styles.nodeLabelFill)
            .style("font",
                   `${styles.nodeLabelFontSize} ${styles.nodeLabelFontFamily}`)
            .attr("text-anchor", "left")
            .attr("alignment-baseline", "hanging")
            .text(label.text);
      });
    }

    // Add ports
    if (node.data.ports) {
      node.data.ports.forEach(function(port) {
        g.appendComment("Port");
        g.append("rect")
            .attr("x", node.data.x + port.x)
            .attr("y", node.data.y + port.y)
            .attr("width", port.width)
            .attr("height", port.height)
            .attr("stroke", styles.portShapeStroke)
            .attr("fill", styles.portShapeFill);
      });
    }

    if (node.data.edges) {
      node.data.edges.forEach(edge => drawEdge(g, edge));
    }

    // Draw child nodes recursively
    if (node.children) {
      node.children.forEach(node => drawNode(g, node));
    }
  }
  // Draw the root node
  drawNode(svg, root);
}

function update(graph) {
  const elk = new ELK()
  elk.layout(graph).then(function(layout) {
    console.log(layout);
    renderGraph(d3.select("svg"), d3.hierarchy(layout, d => d.children));
  });
}

d3.json("graph.json").then(update, function(err) { throw err; })
