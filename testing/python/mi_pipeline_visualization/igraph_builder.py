#!/usr/bin/env python

from recipe_schema import parse_recipe_collection, FluidInput, FluidOutput, ItemInput, ItemOutput, Recipe, ItemModel, FluidModel, MIElectricRecipe
import json
from common import JSON_PATH
from pathlib import Path

from beartype.typing import Any, Dict, List, Optional, Union, Callable, Set
from beartype import beartype
from abc import ABC
from pydantic import BaseModel, Field, field_validator, ValidationError
from pydantic.aliases import AliasChoices
import igraph as ig

from log_writer import log


class FluidNodeData(BaseModel):
    id: str


class ItemNodeData(BaseModel):
    id: str


class RecipeNodeData(BaseModel):
    type: str
    fluid_inputs: List[FluidNodeData]
    fluid_outputs: List[FluidNodeData]
    item_inputs: List[ItemNodeData]
    item_outputs: List[ItemNodeData]
    duration: Optional[int] = None
    eu: Optional[int] = None


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
    def _traverse(obj: Any) -> None:
        if isinstance(obj, Recipe):
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

            fluid_input_nodes = []
            fluid_output_nodes = []
            item_input_nodes = []
            item_output_nodes = []

            for fluid_input in fluid_inputs:
                accepted, fluid_node_data = disambiguate(
                    fluid_input, disambiguate_fluid)
                if not accepted:
                    continue

                if fluid_node_data.id not in fluid_nodes:
                    fluid_nodes[fluid_node_data.id] = graph.vcount()
                    graph.add_vertex(name=fluid_node_data.id,
                                     node_type="fluid",
                                     data=fluid_node_data)

                fluid_input_nodes.append(fluid_node_data)

            for fluid_output in fluid_outputs:
                accepted, fluid_node_data = disambiguate(
                    fluid_output, disambiguate_fluid)
                if not accepted:
                    continue

                if fluid_node_data.id not in fluid_nodes:
                    fluid_nodes[fluid_node_data.id] = graph.vcount()
                    graph.add_vertex(name=fluid_node_data.id,
                                     node_type="fluid",
                                     data=fluid_node_data)

                fluid_output_nodes.append(fluid_node_data)

            for item_input in item_inputs:
                accepted, item_node_data = disambiguate(
                    item_input, disambiguate_item)
                if not accepted:
                    continue

                if item_node_data.id not in item_nodes:
                    item_nodes[item_node_data.id] = graph.vcount()
                    graph.add_vertex(name=item_node_data.id,
                                     node_type="item",
                                     data=item_node_data)

                item_input_nodes.append(item_node_data)

            for item_output in item_outputs:
                accepted, item_node_data = disambiguate(
                    item_output, disambiguate_item)
                if not accepted:
                    continue

                if item_node_data.id not in item_nodes:
                    item_nodes[item_node_data.id] = graph.vcount()
                    graph.add_vertex(name=item_node_data.id,
                                     node_type="item",
                                     data=item_node_data)

                item_output_nodes.append(item_node_data)

            recipe_data = RecipeNodeData(
                type=obj.type,
                fluid_inputs=fluid_input_nodes,
                fluid_outputs=fluid_output_nodes,
                item_inputs=item_input_nodes,
                item_outputs=item_output_nodes,
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
                graph.add_edge(fluid_nodes[fluid_input.id], recipe_vertex, data=fluid_input)

            for fluid_output in recipe_data.fluid_outputs:
                graph.add_edge(recipe_vertex, fluid_nodes[fluid_output.id], data=fluid_output)

            for item_input in recipe_data.item_inputs:
                graph.add_edge(item_nodes[item_input.id], recipe_vertex, data=item_input)

            for item_output in recipe_data.item_outputs:
                graph.add_edge(recipe_vertex, item_nodes[item_output.id], data=item_output)

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
def parse_recipes_to_graph(path: Path) -> ig.Graph:
    content = json.loads(path.read_text())

    collection = dict(recipes=content)
    model = parse_recipe_collection(collection)

    Path("/tmp/model.json").write_text(model.model_dump_json(indent=2, exclude_none=True))
    # exit()

    @beartype
    def disambiguate_fluid(
            model: FluidModel) -> Optional[FluidNodeData | IgnoreResult]:
        if model.fluid:
            return FluidNodeData(id=model.fluid)

        elif model.tag:
            return FluidNodeData(id=model.tag)

        return None

    @beartype
    def disambiguate_item(
            model: ItemModel) -> Optional[ItemNodeData | IgnoreResult]:
        if model.type and model.type.startswith("neoforge:"):
            return IgnoreResult()

        try:
            if model.item:
                return ItemNodeData(id=model.item)

            elif model.tag:
                return ItemNodeData(id=model.tag)

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

    def recipe_callback(data: RecipeNodeData) -> bool:
        if data.type:
            if data.type == "modern_industrialization:chemical_reactor":
                return True

            # split = data.type.split(":")
            # if split[0] in interesting_names:
            #     return True

        # ref = collect_inputs_outputs(data, disambiguate_item=disambiguate_item)

        # for item in itertools.chain(*ref):
        #     if item.modname in interesting_names:
        #         return True

        # return False

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


if __name__ == "__main__":
    parse_recipes_to_graph(Path(JSON_PATH))
