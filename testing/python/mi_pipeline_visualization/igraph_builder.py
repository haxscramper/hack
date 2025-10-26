#!/usr/bin/env python

from recipe_schema import parse_recipe_collection, FluidInput, FluidOutput, ItemInput, ItemOutput, Recipe, ItemModel, FluidModel, MIElectricRecipe
import json
from common import JSON_PATH, TEXTURE_DIRECTORY
from pathlib import Path

from beartype.typing import Any, Dict, List, Optional, Union, Callable, Set, Literal, Type
from beartype import beartype
from abc import ABC
from pydantic import BaseModel, Field, field_validator, ValidationError
from pydantic.aliases import AliasChoices
import igraph as ig
import os
import glob
from collections import defaultdict
import functools

from log_writer import log

_texture_cache = None


def _build_texture_cache():
    """Build a cache of all textures in the texture directory."""
    global _texture_cache

    if _texture_cache is not None:
        return

    _texture_cache = defaultdict(list)

    if not os.path.exists(TEXTURE_DIRECTORY):
        log.warning(f"Texture directory does not exist: {TEXTURE_DIRECTORY}")
        return

    # Find all PNG files matching the pattern
    pattern = os.path.join(TEXTURE_DIRECTORY, "*.png")
    all_textures = glob.glob(pattern)

    for texture_path in all_textures:
        filename = os.path.basename(texture_path)

        # Parse filename: [fluid__]modid__itemid[__metadata][__NBT].png
        parts = filename[:-4].split('__')  # Remove .png extension

        if len(parts) >= 2:
            if parts[0] == 'fluid' and len(parts) >= 3:
                # fluid__modid__itemid[__metadata][__NBT].png
                modid = parts[1]
                itemid = parts[2]
                key = f"fluid:{modid}:{itemid}"
            else:
                # modid__itemid[__metadata][__NBT].png
                modid = parts[0]
                itemid = parts[1]
                key = f"{modid}:{itemid}"

            _texture_cache[key].append(texture_path)

    Path("/tmp/texture-cache.json").write_text(
        json.dumps(_texture_cache, indent=2))


@functools.cache
def get_texture_path(id: str) -> str | None:
    """
    Resolve texture path for given mod:item ID.
    
    Args:
        id: Item ID in format "modid:itemid" or "fluid:modid:itemid"
        
    Returns:
        Path to texture file, or None if not found/ambiguous
    """
    if ':' not in id:
        log.warning(
            f"Invalid ID format '{id}', expected 'modid:itemid' or 'fluid:modid:itemid'"
        )
        return None

    if id.startswith("c:"):
        return None

    # Ensure cache is built
    _build_texture_cache()

    matches = _texture_cache.get(id, [])

    if not matches:
        log.warning(f"No texture found for '{id}' in {TEXTURE_DIRECTORY}")
        return None

    if len(matches) == 1:
        return matches[0]

    # Multiple matches - return first and warn
    # log.warning(
    #     f"Multiple textures found for '{id}', using first: {matches[0]}. All matches: {matches}"
    # )
    return matches[0]


class FluidNodeData(BaseModel):
    id: str
    image: Optional[str] = None
    amount: Optional[int] = None
    node_kind: Literal["fluid"] = "fluid"


class ItemNodeData(BaseModel):
    id: str
    image: Optional[str] = None
    amount: Optional[int] = None
    node_kind: Literal["item"] = "item"


class RecipeNodeData(BaseModel):
    type: str
    machine_item: Optional[ItemNodeData] = None
    node_kind: Literal["recipe"] = "recipe"
    fluid_inputs: List[FluidNodeData]
    fluid_outputs: List[FluidNodeData]
    item_inputs: List[ItemNodeData]
    item_outputs: List[ItemNodeData]
    duration: Optional[int] = None
    eu: Optional[int] = None


NodeDataUnion: Type = Union[RecipeNodeData, FluidNodeData, ItemNodeData]


class IgnoreResult():
    pass


@beartype
def collect_inputs_outputs(
    obj: Any,
    disambiguate_item: Callable[[ItemModel],
                                Optional[ItemNodeData | IgnoreResult]],
) -> tuple[List[FluidInput], List[FluidOutput], List[ItemInput],
           List[ItemOutput]]:
    fluid_inputs = []
    fluid_outputs = []
    item_inputs = []
    item_outputs = []

    @beartype
    def _traverse_for_io(current_obj: Any) -> None:
        if isinstance(current_obj, FluidInput):
            fluid_inputs.append(current_obj)
        elif isinstance(current_obj, FluidOutput):
            fluid_outputs.append(current_obj)
        elif isinstance(current_obj, ItemInput):
            item_inputs.append(current_obj)
        elif isinstance(current_obj, ItemOutput):
            item_outputs.append(current_obj)
        elif isinstance(current_obj, list):
            if current_obj and isinstance(current_obj[0], ItemInput):
                item_node_data = disambiguate_item(current_obj[0])
                if item_node_data:
                    item_inputs.append(current_obj[0])
            elif current_obj and isinstance(current_obj[0], ItemOutput):
                item_node_data = disambiguate_item(current_obj[0])
                if item_node_data:
                    item_outputs.append(current_obj[0])
            else:
                for item in current_obj:
                    _traverse_for_io(item)
        elif isinstance(current_obj, dict):
            for value in current_obj.values():
                _traverse_for_io(value)
        elif hasattr(current_obj, "__dict__"):
            for attr_value in current_obj.__dict__.values():
                _traverse_for_io(attr_value)

    _traverse_for_io(obj)
    return fluid_inputs, fluid_outputs, item_inputs, item_outputs


@beartype
def create_recipe_graph(
    data: Any,
    disambiguate_item: Callable[[ItemModel],
                                Optional[ItemNodeData | IgnoreResult]],
    disambiguate_fluid: Callable[[FluidModel],
                                 Optional[FluidNodeData | IgnoreResult]],
) -> ig.Graph:
    graph = ig.Graph(directed=True)
    recipe_nodes: Dict[str, int] = {}
    item_nodes: Dict[str, int] = {}
    fluid_nodes: Dict[str, int] = {}

    @beartype
    def _add_recipe_node(obj: Recipe):
        fluid_inputs, fluid_outputs, item_inputs, item_outputs = collect_inputs_outputs(
            obj, disambiguate_item)

        def disambiguate(
                value, callback
        ) -> tuple[bool, Optional[FluidNodeData | ItemNodeData]]:
            node_data = callback(value)
            if isinstance(node_data, IgnoreResult):
                return (False, None)

            elif node_data is None:
                log.warning(
                    f"Could not disambiguate: {value.model_dump_json()} {type(value)}"
                )
                return (False, None)

            else:
                return (True, node_data)

        def process_nodes(nodes_list, disambiguate_func, node_dict, node_type):
            result_nodes = []
            for node in nodes_list:
                accepted, node_data = disambiguate(node, disambiguate_func)
                if not accepted:
                    continue

                if node_data.id not in node_dict:
                    node_dict[node_data.id] = graph.vcount()
                    graph.add_vertex(name=node_data.id,
                                     node_type=node_type,
                                     data=node_data)

                result_nodes.append(node_data)
            return result_nodes

        fluid_input_nodes = process_nodes(fluid_inputs, disambiguate_fluid,
                                          fluid_nodes, "fluid")
        fluid_output_nodes = process_nodes(fluid_outputs, disambiguate_fluid,
                                           fluid_nodes, "fluid")
        item_input_nodes = process_nodes(item_inputs, disambiguate_item,
                                         item_nodes, "item")
        item_output_nodes = process_nodes(item_outputs, disambiguate_item,
                                          item_nodes, "item")

        accepted, machine_item = disambiguate(ItemModel(item=obj.type),
                                              disambiguate_item)

        if isinstance(machine_item, IgnoreResult):
            machine_item = None

        recipe_data = RecipeNodeData(
            type=obj.type,
            fluid_inputs=fluid_input_nodes,
            fluid_outputs=fluid_output_nodes,
            item_inputs=item_input_nodes,
            item_outputs=item_output_nodes,
            machine_item=machine_item,
        )

        if isinstance(obj, MIElectricRecipe):
            recipe_data.duration = obj.duration
            recipe_data.eu = obj.eu

        recipe_id = f"recipe_{obj.type}_{len(recipe_nodes)}"
        if recipe_id not in recipe_nodes:
            recipe_nodes[recipe_id] = graph.vcount()
            graph.add_vertex(name=recipe_id,
                             node_type="recipe",
                             data=recipe_data)

        recipe_vertex = recipe_nodes[recipe_id]

        for fluid_input in recipe_data.fluid_inputs:
            graph.add_edge(fluid_nodes[fluid_input.id],
                           recipe_vertex,
                           data=fluid_input)

        for fluid_output in recipe_data.fluid_outputs:
            graph.add_edge(recipe_vertex,
                           fluid_nodes[fluid_output.id],
                           data=fluid_output)

        for item_input in recipe_data.item_inputs:
            graph.add_edge(item_nodes[item_input.id],
                           recipe_vertex,
                           data=item_input)

        for item_output in recipe_data.item_outputs:
            graph.add_edge(recipe_vertex,
                           item_nodes[item_output.id],
                           data=item_output)

    @beartype
    def _traverse(obj: Any) -> None:
        if isinstance(obj, Recipe):
            _add_recipe_node(obj)

        elif isinstance(obj, list):
            for item in obj:
                _traverse(item)
        elif isinstance(obj, dict):
            for value in obj.values():
                _traverse(value)
        elif hasattr(obj, "__dict__"):
            for attr_value in obj.__dict__.values():
                _traverse(attr_value)

    _traverse(data)
    return graph


@beartype
def extract_fluid_ids(graph: ig.Graph) -> Set[str]:
    return {v["name"] for v in graph.vs if v["node_type"] == "fluid"}


@beartype
def extract_item_ids(graph: ig.Graph) -> Set[str]:
    return {v["name"] for v in graph.vs if v["node_type"] == "item"}


@beartype
def filter_recipes(
    graph: ig.Graph,
    recipe_predicate: Optional[Callable[[RecipeNodeData], bool]] = None
) -> ig.Graph:
    vertices_to_keep = set()

    for v in graph.vs:
        if v["node_type"] == "recipe":
            recipe_data = v["data"]
            keep_recipe = True

            if recipe_predicate and not recipe_predicate(recipe_data):
                keep_recipe = False

            if keep_recipe:
                vertices_to_keep.add(v.index)
                vertices_to_keep.update(graph.predecessors(v.index))
                vertices_to_keep.update(graph.successors(v.index))

    vertices_to_remove = set(graph.vs).difference(vertices_to_keep)

    with open("/tmp/filter_recipe.txt", "w") as file:
        for v in vertices_to_remove:
            print(f"{v['node_type']} {v['data']}", file=file)

    return graph.induced_subgraph(list(vertices_to_keep))


import igraph as ig
from enum import Enum


class DependencyMode(Enum):
    IMMEDIATE = "immediate"
    EXCLUDE_NON_PRODUCE = "exclude_non_produce"
    TRANSITIVE = "transitive"


NodeDataUnion = Union["FluidNodeData", "ItemNodeData", "RecipeNodeData"]


def find_dependency_subgraph(
    graph: ig.Graph,
    target_id: str,
    dependency_mode: DependencyMode,
    predicate: Callable[[NodeDataUnion, str, int], bool] = None,
) -> ig.Graph:
    target_vertex = None
    for v in graph.vs:
        if v["node_type"] in ["fluid", "item"] and v["data"].id == target_id:
            target_vertex = v
            break

    if target_vertex is None:
        raise ValueError(f"Target node with id '{target_id}' not found")

    if predicate is None:
        predicate = lambda node_data, context, distance: True

    mst_vertices = set()
    queue = [(target_vertex.index, "target", 0)]
    visited = set()
    distances = {}

    while queue:
        current_idx, context, distance = queue.pop(0)
        if current_idx in visited:
            continue

        current_vertex = graph.vs[current_idx]
        if not predicate(current_vertex["data"], context, distance):
            continue

        visited.add(current_idx)
        mst_vertices.add(current_idx)
        distances[current_idx] = distance

        for edge in graph.es.select(_source=current_idx):
            target_idx = edge.target
            if target_idx not in visited:
                target_vertex_data = graph.vs[target_idx]
                if target_vertex_data["node_type"] == "recipe":
                    queue.append(
                        (target_idx, "recipe_consuming", distance + 1))
                else:
                    queue.append(
                        (target_idx, "output_of_recipe", distance + 1))

    oil_based_vertices = mst_vertices.copy()
    reachable_vertices = mst_vertices.copy()

    if dependency_mode == DependencyMode.IMMEDIATE:
        additional_vertices = set()
        for v_idx in mst_vertices:
            if graph.vs[v_idx]["node_type"] == "recipe":
                for edge in graph.es.select(_target=v_idx):
                    source_vertex = graph.vs[edge.source]
                    recipe_distance = distances.get(v_idx, 0)
                    if predicate(source_vertex["data"], "input_of_recipe",
                                 recipe_distance):
                        additional_vertices.add(edge.source)
                        distances[edge.source] = recipe_distance
        reachable_vertices.update(additional_vertices)

    elif dependency_mode == DependencyMode.EXCLUDE_NON_PRODUCE:
        additional_vertices = set()
        for v_idx in mst_vertices:
            if graph.vs[v_idx]["node_type"] == "recipe":
                for edge in graph.es.select(_target=v_idx):
                    if edge.source in oil_based_vertices:
                        source_vertex = graph.vs[edge.source]
                        recipe_distance = distances.get(v_idx, 0)
                        if predicate(source_vertex["data"], "input_of_recipe",
                                     recipe_distance):
                            additional_vertices.add(edge.source)
                            distances[edge.source] = recipe_distance
        reachable_vertices.update(additional_vertices)

    elif dependency_mode == DependencyMode.TRANSITIVE:
        changed = True
        while changed:
            changed = False
            current_size = len(reachable_vertices)
            additional_vertices = set()

            for v_idx in reachable_vertices:
                if graph.vs[v_idx]["node_type"] == "recipe":
                    for edge in graph.es.select(_target=v_idx):
                        if edge.source not in reachable_vertices:
                            source_vertex = graph.vs[edge.source]
                            recipe_distance = distances.get(v_idx, 0)
                            if predicate(source_vertex["data"],
                                         "input_of_recipe", recipe_distance):
                                additional_vertices.add(edge.source)
                                distances[edge.source] = recipe_distance

            reachable_vertices.update(additional_vertices)

            for v_idx in additional_vertices:
                if graph.vs[v_idx]["node_type"] in ["fluid", "item"]:
                    queue = [(v_idx, "dependency", distances.get(v_idx, 0))]
                    visited_trans = set()

                    while queue:
                        current_idx, context, distance = queue.pop(0)
                        if current_idx in visited_trans:
                            continue

                        current_vertex = graph.vs[current_idx]
                        if not predicate(current_vertex["data"], context,
                                         distance):
                            continue

                        visited_trans.add(current_idx)
                        reachable_vertices.add(current_idx)

                        for edge in graph.es.select(_source=current_idx):
                            target_idx = edge.target
                            if target_idx not in visited_trans:
                                target_vertex_data = graph.vs[target_idx]
                                new_distance = distance + 1
                                distances[target_idx] = new_distance
                                if target_vertex_data["node_type"] == "recipe":
                                    queue.append(
                                        (target_idx, "recipe_consuming",
                                         new_distance))
                                else:
                                    queue.append(
                                        (target_idx, "output_of_recipe",
                                         new_distance))

            if len(reachable_vertices) > current_size:
                changed = True

    subgraph = graph.subgraph(list(reachable_vertices))
    return subgraph


@beartype
def filter_fluids(
        graph: ig.Graph, fluid_predicate: Callable[[FluidNodeData],
                                                   bool]) -> ig.Graph:
    vertices_to_keep = []

    for v in graph.vs:
        if v["node_type"] == "fluid":
            if fluid_predicate(v["data"]):
                vertices_to_keep.append(v.index)
        else:
            vertices_to_keep.append(v.index)

    return graph.induced_subgraph(vertices_to_keep)


@beartype
def filter_items(graph: ig.Graph, item_predicate: Callable[[ItemNodeData],
                                                           bool]) -> ig.Graph:
    vertices_to_keep = []

    for v in graph.vs:
        if v["node_type"] == "item":
            if item_predicate(v["data"]):
                vertices_to_keep.append(v.index)
        else:
            vertices_to_keep.append(v.index)

    return graph.induced_subgraph(vertices_to_keep)


@beartype
def remove_isolated_nodes(graph: ig.Graph) -> ig.Graph:
    nodes_to_keep = [v.index for v in graph.vs if graph.degree(v.index) > 0]
    return graph.subgraph(nodes_to_keep)


@beartype
def find_recipes_graph(result: ig.Graph, machine_id: str) -> ig.Graph:

    def recipe_callback(data: RecipeNodeData) -> bool:
        if data.type:
            if data.type == machine_id:
                return True

    log.info(
        f"Constructed initial graph, {result.vcount()} vertices {result.ecount()} edges"
    )
    result = filter_recipes(result, recipe_predicate=recipe_callback)
    log.info(
        f"Filtered MI recipes, {result.vcount()} vertices {result.ecount()} edges"
    )
    result = remove_isolated_nodes(result)
    log.info(
        f"Removed isolated nodes, {result.vcount()} vertices {result.ecount()} edges"
    )

    return result


@beartype
def parse_recipes_to_graph(path: Path) -> ig.Graph:
    content = json.loads(path.read_text())

    collection = dict(recipes=content)
    model = parse_recipe_collection(collection)

    Path("/tmp/model.json").write_text(
        model.model_dump_json(indent=2, exclude_none=True))
    # exit()

    @beartype
    def disambiguate_fluid(
            model: FluidModel) -> Optional[FluidNodeData | IgnoreResult]:
        if model.fluid:
            return FluidNodeData(
                id=model.fluid,
                image=get_texture_path("fluid:" + model.fluid),
                amount=model.amount,
            )

        elif model.tag:
            return FluidNodeData(
                id=model.tag,
                amount=model.amount,
            )

        return None

    @beartype
    def disambiguate_item(
            model: ItemModel) -> Optional[ItemNodeData | IgnoreResult]:
        if model.type and model.type.startswith("neoforge:"):
            return IgnoreResult()

        try:
            if model.item:
                return ItemNodeData(
                    id=model.item,
                    image=get_texture_path(model.item),
                    amount=model.amount,
                )

            elif model.tag:
                return ItemNodeData(
                    id=model.tag,
                    amount=model.amount,
                )

            else:
                return None

        except ValidationError as err:
            log.error(f"{model.model_dump_json()}", exc_info=err)
            return None

    result = create_recipe_graph(
        model,
        disambiguate_fluid=disambiguate_fluid,
        disambiguate_item=disambiguate_item,
    )

    interesting_names = [
        "extended_industrialization",
        "modern_industrialization",
        "industrialization_overdrive",
    ]

    return result


if __name__ == "__main__":
    parse_recipes_to_graph(Path(JSON_PATH))
