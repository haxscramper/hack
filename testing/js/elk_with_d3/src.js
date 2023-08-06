function toSvgPaths(edge) {
  return edge.sections.map(section => {
    const data =
        [ section.startPoint, ...(section.bendPoints || []), section.endPoint ];
    return d3.line().x(d => d.x).y(d => d.y)(data)
  });
}

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

function renderGraph(g, scaleFactor = 1.0) {
  const styles = defaultStyles();
  const edgeShape = edge_path => `
      <path 
        d="${edge_path}"
        stroke="${styles.edgeShapeStroke}"
        fill="${styles.edgeShapeFill}"
        marker-end="url(#edgeShapeMarker)"
      />
    `;

  const edge = edge_d => `
      <g>${toSvgPaths(edge_d).map(edge_path => edgeShape(edge_path))}</g>
    `;

  const edgeDefs = () => `
      <marker 
        id="edgeShapeMarker"
        markerWidth="10"
        markerHeight="10"
        refX="6"
        refY="3"
        orient="auto"
        markerUnits="strokeWidth">
        <path 
          d="M0,0 L0,6 L6,3 z"
          fill="${styles.edgeShapeStroke}"
        />
      </marker>
    `;

  const nodeShape = node_d => `
      <rect 
        x="${node_d.x}" 
        y="${node_d.y}"
        width="${node_d.width}" 
        height="${node_d.height}" 
        stroke="${styles.nodeShapeStroke}" 
        fill="${styles.nodeShapeFill}"
      />
    `;

  const nodeLabel = (label_d, node_d) => `
      <text
        x="${node_d.x + label_d.x}"
        y="${node_d.y + label_d.y}"
        stroke="${styles.nodeLabelStroke}"
        fill="${styles.nodeLabelFill}"
        style="font: ${styles.nodeLabelFontSize} ${styles.nodeLabelFontFamily};"
        text-anchor="left"
        alignment-baseline="hanging">
        ${label_d.text}
      </text>
    `;

  const nodePort = (port_d, node_d) => `
      <rect 
        x="${node_d.x + port_d.x}" 
        y="${node_d.y + port_d.y}" 
        width="${port_d.width}" 
        height="${port_d.height}" 
        stroke="${styles.portShapeStroke}" 
        fill="${styles.portShapeFill}"
      />
    `;

  const node = (node_d, parent_d) => `
      <g style="transform: translate(${parent_d.x}px, ${parent_d.y}px);">
        <g>${nodeShape(node_d)}<g>
        <g>${node_d.labels.map(label_d => nodeLabel(label_d, node_d))}</g>
        <g>${node_d.ports.map(port_d => nodePort(port_d, node_d))}</g>
        <g>${
      node_d.children &&
      node_d.children.map(_node_d => node(_node_d, node_d))}</g>
        <g style="transform: translate(${node_d.x}px, ${node_d.y}px);">
          ${node_d.edges && node_d.edges.map(edge_d => edge(edge_d))}
        </g>
      </g>
    `;

  const diagram = `
      <svg 
        viewBox="${g.x} ${g.y} ${g.width} ${g.height}"
        width="${g.width * scaleFactor}" 
        height="${g.height * scaleFactor}">
        <defs>${edgeDefs()}</defs>
        <g>${g.children.map(node_d => node(node_d, g))}</g>
        <g>${g.edges && g.edges.map(edge_d => edge(edge_d))}</g>
      </svg>
     `;

  return diagram;
}

function update(graph) {
  const elk = new ELK()
  elk.layout(graph).then(function(layout) {
    var svg = d3.select("svg");
    console.log(layout);
    svg.html(renderGraph(layout));
  });
}

d3.json("graph.json").then(update, function(err) { throw err; })
