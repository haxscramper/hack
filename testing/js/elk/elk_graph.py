#!/usr/bin/env python
import subprocess
import json
import os

import logging


logging.basicConfig(level=logging.DEBUG, format="%(asctime)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s")


# Step 1: Execute the JS code
result = subprocess.run(
    ["node", "elk_graph.js"],
    capture_output=True,
    text=True,
)

logging.info(result.stderr)
logging.info(result.stdout)

# Parse the output as JSON
graph = json.loads(result.stdout)

with open("/tmp/result.json", "w") as file:
    file.write(json.dumps(graph, indent=2))


# Step 2: Convert the JSON output to DOT
def to_dot(graph):
    dot = ["digraph {", "splines=ortho;", "node[shape=rect];"]
    for node in graph["children"]:
        pos = f'{node["x"]},{node["y"]}'
        if "ports" in node:
            for port in node["ports"]:
                port_id = port["id"]
                port_pos = f'{node["x"] + port["x"]},{node["y"] + port["y"]}'
                dot.append(
                    f'{port_id} [width=0.1, height=0.1, pos="{port_pos}!", shape=point]'
                )
        dot.append(
            f'{node["id"]} [pos="{pos}!", width={node["width"]/72}, height={node["height"]/72}]'
        )
    for edge in graph["edges"]:
        for section in edge["sections"]:
            points = [f'{section["startPoint"]["x"]},{section["startPoint"]["y"]}']
            if "bendPoints" in section:
                points.extend(
                    [f'{point["x"]},{point["y"]}' for point in section["bendPoints"]]
                )
            points.append(f'e,{section["endPoint"]["x"]},{section["endPoint"]["y"]}')
            dot.append(
                f'  {edge["sources"][0]} -> {edge["targets"][0]} [pos="{" ".join(points)}", arrowhead=none];'
            )

    dot.append("}")
    return "\n".join(dot)


# Write the DOT content to /tmp/result.dot
with open("/tmp/result.dot", "w") as f:
    f.write(to_dot(graph))

# Step 3: Generate PNG using Graphviz
os.system("neato -n -Tpng -o /tmp/result.png /tmp/result.dot")
