const ELK = require("elkjs")
const elk = new ELK()

// clang-format off
const graph = {
  "id": "root",
  "layoutOptions": {
    "elk.algorithm": "layered",
    "elk.layered.feedbackEdges": true,
    "elk.hierarchyHandling": "SEPARATE_CHILDREN",
    "elk.alignment": "RIGHT",
    "elk.direction": "RIGHT",
    "elk.aspectRatio": 10,
    "elk.edgeRouting": "ORTHOGONAL",
    "elk.layered.nodePlacement.bk.fixedAlignment": "BALANCED",
    "elk.layered.allowNonFlowPortsToSwitchSides": true,
    "elk.spacing.edgeNode": 30,
    "elk.spacing.nodeNode": 10,
    "partitioning.activate": true,
    "nodeFlexibility": "NODE_SIZE"
  },
  "children": [
    {"id": "n1", "width": 10, "height": 10, "type": "start"},
    {"id": "n2", "width": 70, "height": 50, "type": "activity"},
    {
      "id": "n3",
      "width": 30,
      "height": 30,
      "type": "gateway",
      "ports": [
        { "id": "P1", "width": 5, "height": 5, "properties": { "port.side": "NORTH", "port.index": "1", "allowNonFlowPortsToSwitchSides": true } },
        { "id": "P2", "width": 5, "height": 5, "properties": { "port.side": "EAST", "port.index": "2", "allowNonFlowPortsToSwitchSides": true } },
        { "id": "P3", "width": 5, "height": 5, "properties": { "port.side": "NORTH", "port.index": "3", "allowNonFlowPortsToSwitchSides": true } },
        { "id": "P4", "width": 5, "height": 5, "properties": { "port.side": "WEST", "port.index": "4", "allowNonFlowPortsToSwitchSides": true } }
      ],
      "properties": { "portConstraints": "FIXED_ORDER", "portAlignment": "CENTER" }
    },
    {"id": "n4", "width": 70, "height": 50, "type": "activity"},
    {
      "id": "n5",
      "width": 70,
      "height": 50,
      "type": "activity",
      "ports": [
        { "id": "P17", "width": 5, "height": 5, "properties": {"port.side": "NORTH", "port.index": "1"} },
        { "id": "P18", "width": 5, "height": 5, "properties": {"port.side": "SOUTH", "port.index": "2"} },
        { "id": "P19", "width": 5, "height": 5, "properties": {"port.side": "EAST", "port.index": "3"} },
        { "id": "P20", "width": 5, "height": 5, "properties": {"port.side": "WEST", "port.index": "4"} }
      ],
      "properties": {"portConstraints": "FIXED_SIDE", "portAlignment": "CENTER"}
    },
    {"id": "n6", "width": 70, "height": 50, "type": "activity"}
  ],
  "edges": [
    {"id": "e1", "sources": ["n1"], "targets": ["n2"]},
    {"id": "e2", "sources": ["n2"], "targets": ["P4"]},
    {"id": "e15", "sources": ["P1"], "targets": ["n4"]},
    {"id": "e16", "sources": ["P2"], "targets": ["n5"]},
    {"id": "e17", "sources": ["P3"], "targets": ["n6"]}
  ]
};
// clang-format on

elk.layout(graph)
    .then(function(res) { console.log(JSON.stringify(res)); })
    .catch(console.error)
