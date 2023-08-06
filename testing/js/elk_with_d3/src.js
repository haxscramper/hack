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

const styles = defaultStyles();

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
      "transform", `translate(${node.parent.data.x}, ${node.parent.data.y})`);
  g.appendComment("Node");

  // Add a rectangle for the node
  g.append("rect")
      .attr("x", node.data.x)
      .attr("y", node.data.y)
      .attr("width", node.data.width)
      .attr("height", node.data.height)
      .attr("stroke", styles.nodeShapeStroke)
      .attr("fill", styles.nodeShapeFill);

  const labels = g.append("g");
  labels.appendComment("Title");

  // Add labels
  if (node.data.labels) {
    node.data.labels.forEach((label) => {
      labels.append("text")
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

  const ports = g.append("g");
  ports.appendComment("Ports");

  // Add ports
  if (node.data.ports) {
    node.data.ports.forEach(function(port) {
      ports.append("rect")
          .attr("x", node.data.x + port.x)
          .attr("y", node.data.y + port.y)
          .attr("width", port.width)
          .attr("height", port.height)
          .attr("stroke", styles.portShapeStroke)
          .attr("fill", styles.portShapeFill);
    });
  }

  const edges = g.append("g").attr("transform",
                                   `translate(${node.data.x}, ${node.data.y})`);
  edges.appendComment("Edges");

  if (node.data.edges) {
    node.data.edges.forEach(edge => drawEdge(edges, edge));
  }

  const nodes = g.append("g");
  nodes.appendComment("Nodes");

  // Draw child nodes recursively
  if (node.children) {
    node.children.forEach(node => drawNode(nodes, node));
  }
}

function update(graph) {
  const elk = new ELK()
  elk.layout(graph).then(function(layout) {
    var root = d3.hierarchy(layout, d => d.children);
    var svg = d3.select("svg");
    console.log(root);

    svg.attr("width", root.data.width)
        .attr("height", root.data.height)
        .attr("viewBox", `${root.data.x} ${root.data.y} ${root.data.width} ${
                             root.data.height}`);

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

    for (var node of root.children) {
      drawNode(svg, node);
    }

    for (var edge of root.data.edges) {
      drawEdge(svg, edge);
    }
  });
}

d3.json("graph.json").then(update, function(err) { throw err; })
