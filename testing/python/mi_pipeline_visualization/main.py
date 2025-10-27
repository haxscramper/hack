#!/usr/bin/env python

from log_writer import log
from common import JSON_PATH, USE_GRAPH_CACHE, ARANGO_PORT, ARANGO_USER, ARANGO_PASSWORD, ARANGO_GRAPH_NAME, USE_ARANGO_CACHE, ARANGO_QUERY_FILE
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


def upload_graph_to_arango(graph: ig.Graph) -> None:
    from arango import ArangoClient
    from arango.exceptions import ArangoError
    
    client = ArangoClient(hosts=f"http://localhost:{ARANGO_PORT}")
    
    try:
        db = client.db("_system", username=ARANGO_USER, password=ARANGO_PASSWORD)
    except ArangoError as e:
        raise Exception(f"Failed to connect to ArangoDB: {e}")
    
    if USE_ARANGO_CACHE and db.has_graph(ARANGO_GRAPH_NAME):
        return
    
    if db.has_graph(ARANGO_GRAPH_NAME):
        log.info(f"Dropping previous graph {ARANGO_GRAPH_NAME}")
        db.delete_graph(ARANGO_GRAPH_NAME, ignore_missing=True, drop_collections=True)
    
    arango_graph = db.create_graph(ARANGO_GRAPH_NAME)
    
    if not db.has_collection("vertices"):
        db.create_collection("vertices")
    arango_graph.create_vertex_collection("vertices")
    
    if not db.has_collection("edges"):
        db.create_collection("edges", edge=True)
    arango_graph.create_edge_definition(
        edge_collection="edges",
        from_vertex_collections=["vertices"],
        to_vertex_collections=["vertices"]
    )
    
    vertices_data = []
    for vertex in graph.vs:
        vertex_data = {
            "_key": str(vertex.index),
            "node_type": vertex["node_type"],
            "data": vertex["data"].model_dump()
        }
        vertices_data.append(vertex_data)
    
    if vertices_data:
        db.collection("vertices").import_bulk(vertices_data)
    
    edges_data = []
    for edge in graph.es:
        edge_data = {
            "_from": f"vertices/{edge.source}",
            "_to": f"vertices/{edge.target}"
        }
        edges_data.append(edge_data)
    
    if edges_data:
        db.collection("edges").import_bulk(edges_data)

def run_arango_query() -> list:
    from arango import ArangoClient
    from arango.exceptions import ArangoError
    from pathlib import Path
    
    client = ArangoClient(hosts=f"http://localhost:{ARANGO_PORT}")
    
    try:
        db = client.db("_system", username=ARANGO_USER, password=ARANGO_PASSWORD)
    except ArangoError as e:
        raise Exception(f"Failed to connect to ArangoDB: {e}")
    
    query_path = Path(ARANGO_QUERY_FILE)
    if not query_path.is_absolute():
        query_path = Path(__file__).parent / query_path
    
    query_content = query_path.read_text()
    
    cursor = db.aql.execute(query_content)
    results = list(cursor)
    
    return results

def convert_arango_results_to_igraph(arango_results: list) -> ig.Graph:
    from typing import Dict, Any
    
    vertices: Dict[str, Dict[str, Any]] = {}
    edges: list = []
    
    for result in arango_results:
        if "vertices" in result:
            for vertex_data in result["vertices"]:
                key = vertex_data["_key"]
                node_type = vertex_data["node_type"]
                
                if node_type == "fluid":
                    data_obj = gbuild.FluidNodeData(**vertex_data["data"])
                elif node_type == "item":
                    data_obj = gbuild.ItemNodeData(**vertex_data["data"])
                elif node_type == "recipe":
                    data_obj = gbuild.RecipeNodeData(**vertex_data["data"])
                else:
                    raise ValueError(f"Unknown node type: {node_type}")
                
                vertices[key] = {
                    "node_type": node_type,
                    "data": data_obj
                }
        
        if "edges" in result:
            for edge_data in result["edges"]:
                source_key = edge_data["_from"].split("/")[1]
                target_key = edge_data["_to"].split("/")[1]
                edges.append((source_key, target_key))
    
    graph = ig.Graph()
    
    vertex_keys = list(vertices.keys())
    graph.add_vertices(len(vertex_keys))
    
    key_to_index = {key: i for i, key in enumerate(vertex_keys)}
    
    for i, key in enumerate(vertex_keys):
        graph.vs[i]["node_type"] = vertices[key]["node_type"]
        graph.vs[i]["data"] = vertices[key]["data"]
    
    edge_tuples = [(key_to_index[source], key_to_index[target]) for source, target in edges if source in key_to_index and target in key_to_index]
    graph.add_edges(edge_tuples)
    
    return graph

cachefile = Path("/tmp/mi_recipes.bin")

def get_initial_graph(): 
    if USE_GRAPH_CACHE and cachefile.exists():
        log.info(f"Cache file {cachefile} exists, reading the igraph")
        with cachefile.open("rb") as f:
            graph: ig.Graph = pickle.load(f)

    else:
        graph = gbuild.parse_recipes_to_graph(Path(JSON_PATH))

        with cachefile.open("wb") as f:
            pickle.dump(graph, f)

    return graph

def filter_graph(graph: ig.Graph) -> ig.Graph: 
    upload_graph_to_arango(graph)
    results = run_arango_query()
    return convert_arango_results_to_igraph(results)

    log.info(f"Using graph with {graph.ecount()} edges and {graph.vcount()} nodes")

def main():
    init_graph = get_initial_graph()
    graph = filter_graph(init_graph)
    result = convert_to_elk(graph)

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


if __name__ == "__main__":
    main()
