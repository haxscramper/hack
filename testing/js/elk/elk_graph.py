#!/usr/bin/env python
import subprocess
import json
import os

# Step 1: Execute the JS code
result = subprocess.run(
    ["node", "elk_graph.js"],
    capture_output=True,
    text=True,
)

# Parse the output as JSON
graph = json.loads(result.stdout)

with open("/tmp/result.json", "w") as file:
    file.write(json.dumps(graph, indent=2))

# Step 2: Convert the JSON output to DOT
dot_lines = ["digraph {", "splines=ortho;"]
for node in graph["children"]:
    dot_lines.append(
        f'  {node["id"]} [pos="{node["x"]},{node["y"]}!", width=0.3937, height=0.3937, shape=rect];'
    )

for edge in graph["edges"]:
    for section in edge["sections"]:
        points = [f'{section["startPoint"]["x"]},{section["startPoint"]["y"]}']
        if "bendPoints" in section:
            points.extend(
                [f'{point["x"]},{point["y"]}' for point in section["bendPoints"]]
            )
        points.append(f'e,{section["endPoint"]["x"]},{section["endPoint"]["y"]}')
        dot_lines.append(
            f'  {edge["sources"][0]} -> {edge["targets"][0]} [pos="{" ".join(points)}", arrowhead=none];'
        )

dot_lines.append("}")

# Write the DOT content to /tmp/result.dot
with open("/tmp/result.dot", "w") as f:
    f.write("\n".join(dot_lines))

# Step 3: Generate PNG using Graphviz
os.system("neato -n -Tpng -o /tmp/result.png /tmp/result.dot")
