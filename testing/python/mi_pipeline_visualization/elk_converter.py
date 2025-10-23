#!/usr/bin/env python

from log_writer import log
from pathlib import Path
from igraph_builder import parse_recipes_to_graph, RecipeNodeData, ItemNodeData, FluidNodeData
import pickle
import igraph as ig
import elk_schema as elk

USE_CACHE = False

if __name__ == "__main__":
    cachefile = Path("/tmp/mi_recipes.bin")

    if USE_CACHE and cachefile.exists():
        log.info(f"Cache file {cachefile} exists, reading the igraph")
        with cachefile.open("rb") as f:
            graph: ig.Graph = pickle.load(f)

    else:
        graph = parse_recipes_to_graph(
            Path(
                "/home/haxscramper/.local/share/multimc/instances/1.21.1 V2/.minecraft/kubejs/server_scripts/all_recipes.json"
            ))

        with cachefile.open("wb") as f:
            pickle.dump(graph, f)

    log.info(
        f"Using graph with {graph.ecount()} edges and {graph.vcount()} nodes")

    result = elk.Graph(id="root")

    for recipe_idx, v in enumerate(graph.vs):
        match v["node_type"]:
            case "recipe":
                rec: RecipeNodeData = v["data"]

                node = elk.Node(
                    id=f"{rec.type}-{recipe_idx}",
                    width=200,
                    height=100,
                )

                for item_in in rec.fluid_inputs + rec.item_inputs:
                    edge = elk.Edge(id=f"{item_in}")

                result.children.append(node)

    Path("/tmp/elk-init.json").write_text(
        result.model_dump_json(indent=2, exclude_none=True))
    layout = elk.perform_graph_layout(result)
    Path("/tmp/elk-layout.json").write_text(
        layout.model_dump_json(indent=2, exclude_none=True))
