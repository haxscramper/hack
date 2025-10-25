#!/usr/bin/env python

import graphviz
from common import JSON_PATH
from beartype.typing import Dict, List, Tuple, Union
from beartype import beartype
import igraph as ig
from igraph_builder import RecipeNodeData, FluidNodeData, ItemNodeData, parse_recipes_to_graph
from pathlib import Path

def convert_igraph_to_graphviz(graph: ig.Graph) -> graphviz.Digraph:
    dot = graphviz.Digraph()
    dot.attr(rankdir="LR")
    
    for vertex in graph.vs:
        node_data = vertex["data"]
        node_id = str(vertex.index)
        
        if isinstance(node_data, RecipeNodeData):
            inputs = []
            outputs = []

            def norm(s: str) -> str:
                return str(s).replace(":", "_")
            
            for fluid_input in node_data.fluid_inputs:
                inputs.append(f"<f_in_{norm(fluid_input.id)}>F: {norm(fluid_input.id)}")
            
            for item_input in node_data.item_inputs:
                inputs.append(f"<i_in_{norm(item_input.id)}>I: {norm(item_input.id)}")
            
            for fluid_output in node_data.fluid_outputs:
                outputs.append(f"<f_out_{norm(fluid_output.id)}>F: {norm(fluid_output.id)}")
            
            for item_output in node_data.item_outputs:
                outputs.append(f"<i_out_{norm(item_output.id)}>I: {norm(item_output.id)}")
            
            label_parts = []
            if inputs:
                label_parts.append("|".join(inputs))
            
            recipe_info = f"Recipe: {node_data.type}"
            if node_data.duration is not None:
                recipe_info += f" ({node_data.duration}s)"
            if node_data.eu is not None:
                recipe_info += f" {node_data.eu} EU"
            label_parts.append(recipe_info)
            
            if outputs:
                label_parts.append("|".join(outputs))
            
            label = "{" + "|".join(label_parts) + "}"
            dot.node(node_id, label=label, shape="record")
            
        elif isinstance(node_data, (FluidNodeData, ItemNodeData)):
            dot.node(node_id, label=node_data.id, shape="box")
    
    for edge in graph.es:
        source_vertex = graph.vs[edge.source]
        target_vertex = graph.vs[edge.target]
        source_data = source_vertex["data"]
        target_data = target_vertex["data"]
        
        source_id = str(edge.source)
        target_id = str(edge.target)
        
        if isinstance(source_data, (FluidNodeData, ItemNodeData)) and isinstance(target_data, RecipeNodeData):
            port = None
            if isinstance(source_data, FluidNodeData):
                for fluid_input in target_data.fluid_inputs:
                    if fluid_input.id == source_data.id:
                        port = f"f_in_{norm(fluid_input.id)}"
                        break
            elif isinstance(source_data, ItemNodeData):
                for item_input in target_data.item_inputs:
                    if item_input.id == source_data.id:
                        port = f"i_in_{norm(item_input.id)}"
                        break
            
            if port:
                dot.edge(source_id, f"{target_id}:{port}")
            else:
                dot.edge(source_id, target_id)
                
        elif isinstance(source_data, RecipeNodeData) and isinstance(target_data, (FluidNodeData, ItemNodeData)):
            port = None
            if isinstance(target_data, FluidNodeData):
                for fluid_output in source_data.fluid_outputs:
                    if fluid_output.id == target_data.id:
                        port = f"f_out_{norm(fluid_output.id)}"
                        break
            elif isinstance(target_data, ItemNodeData):
                for item_output in source_data.item_outputs:
                    if item_output.id == target_data.id:
                        port = f"i_out_{norm(item_output.id)}"
                        break
            
            if port:
                dot.edge(f"{source_id}:{port}", target_id)
            else:
                dot.edge(source_id, target_id)
    
    return dot

if __name__ == "__main__":
    graph = parse_recipes_to_graph(
        Path(
            JSON_PATH
        ))

    dot = convert_igraph_to_graphviz(graph)

    dot.render("/tmp/result.dot", engine="sfdp")
