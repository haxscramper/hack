#!/usr/bin/env python

from log_writer import log
from common import JSON_PATH, USE_GRAPH_CACHE
import igraph as ig
from beartype import beartype
from pathlib import Path
import pickle
from plumbum import local, CommandNotFound

import igraph_builder as gbuild
from elk_converter import convert_to_elk, graph_to_typst
from typst_schema import generate_typst
import elk_schema as elk
from graphviz_converter import convert_igraph_to_graphviz

cachefile = Path("/tmp/mi_recipes.bin")

if USE_GRAPH_CACHE and cachefile.exists():
    log.info(f"Cache file {cachefile} exists, reading the igraph")
    with cachefile.open("rb") as f:
        graph: ig.Graph = pickle.load(f)

else:
    graph = gbuild.parse_recipes_to_graph(Path(JSON_PATH))

    with cachefile.open("wb") as f:
        pickle.dump(graph, f)


def accept_node(data: gbuild.NodeDataUnion, context: str, distance) -> bool:
    return distance < 8


def continue_predicate(data: gbuild.NodeDataUnion, context: gbuild.PredicateContext) -> bool:
    if isinstance(data, gbuild.FluidNodeData) and data.id == "modern_industrialization:oxygen":
        return False

    else:
        return True

graph = gbuild.find_dependency_subgraph(
    graph,
    "modern_industrialization:crude_oil",
    "immediate",
    node_predicate=accept_node,
)

log.info(f"Using graph with {graph.ecount()} edges and {graph.vcount()} nodes")

result = convert_to_elk(graph)

# gv_graph = convert_igraph_to_graphviz(graph)
# gv_graph.render("/tmp/result.dot")

elk_init = Path("/tmp/elk-init.json")
elk_init.write_text(result.model_dump_json(indent=2, exclude_none=True))
log.info(f"Wrote initial graph structure to {elk_init}")
layout = elk.perform_graph_layout(result)
elk_layout = Path("/tmp/elk-layout.json")
log.info(f"Wrote graph layout JSON to {elk_layout}")
elk_layout.write_text(
    layout.model_dump_json(indent=2, exclude_none=True, exclude_unset=False))

doc = graph_to_typst(layout)
doc_json = Path("/tmp/typst-doc.json")
doc_json.write_text(doc.model_dump_json(indent=2))
log.info(f"Wrote doc JSON to {doc_json}")
final = generate_typst(doc)
final_path = Path("/tmp/result.typ")
log.info(f"Write final text to {final_path}")
final_path.write_text(final)

try:
    fmt = local["typstyle"]
    fmt.run(["--inplace", str(final_path)])

except CommandNotFound:
    log.warning(
        f"Could not find commands `typstyle` -- install it for auto-formatting `.typ` file after creation"
    )
