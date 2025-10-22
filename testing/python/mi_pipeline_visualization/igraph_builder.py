#!/usr/bin/env python

from recipe_schema import parse_recipe_collection, FluidInput, FluidOutput, ItemInput, ItemOutput, Recipe, ItemModel, FluidModel
import json
from pathlib import Path

from beartype.typing import Any, Dict, List, Optional, Union, Callable, Set
from beartype import beartype
from abc import ABC
from pydantic import BaseModel, Field, field_validator, ValidationError
from pydantic.aliases import AliasChoices
import igraph as ig
import structlog

from rich.console import Console
from rich.traceback import Traceback

structlog.configure(
    processors=[
        structlog.stdlib.filter_by_level,
        structlog.stdlib.add_logger_name,
        structlog.stdlib.add_log_level,
        structlog.stdlib.PositionalArgumentsFormatter(),
        structlog.processors.TimeStamper(fmt="iso"),
        structlog.processors.StackInfoRenderer(),
        structlog.dev.ConsoleRenderer(
            colors=True,
            exception_formatter=structlog.dev.RichTracebackFormatter(width=-1),
            force_colors=True,
        )
    ],
    wrapper_class=structlog.stdlib.BoundLogger,
    logger_factory=structlog.stdlib.LoggerFactory(),
    cache_logger_on_first_use=True,
)

console = Console(width=None, force_terminal=True)

log = structlog.get_logger()


class RecipeNodeData(BaseModel):
    type: str
    fluid_inputs: List[FluidInput]
    fluid_outputs: List[FluidOutput]
    item_inputs: List[ItemInput]
    item_outputs: List[ItemOutput]


class FluidNodeData(BaseModel):
    id: str


class ItemNodeData(BaseModel):
    id: str


@beartype
def create_recipe_graph(
    data: Any, disambiguate_item: Callable[[ItemModel],
                                           Optional[ItemNodeData]],
    disambiguate_fluid: Callable[[FluidModel], Optional[FluidNodeData]]
) -> ig.Graph:
    graph = ig.Graph(directed=True)
    recipe_nodes: Dict[str, int] = {}
    item_nodes: Dict[str, int] = {}
    fluid_nodes: Dict[str, int] = {}

    @beartype
    def _collect_inputs_outputs(
        obj: Any
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
    def _traverse(obj: Any) -> None:
        if isinstance(obj, Recipe):
            fluid_inputs, fluid_outputs, item_inputs, item_outputs = _collect_inputs_outputs(
                obj)

            recipe_data = RecipeNodeData(type=obj.type,
                                         fluid_inputs=fluid_inputs,
                                         fluid_outputs=fluid_outputs,
                                         item_inputs=item_inputs,
                                         item_outputs=item_outputs)

            recipe_id = f"recipe_{obj.type}_{len(recipe_nodes)}"
            if recipe_id not in recipe_nodes:
                recipe_nodes[recipe_id] = graph.vcount()
                graph.add_vertex(name=recipe_id,
                                 node_type="recipe",
                                 data=recipe_data)

            recipe_vertex = recipe_nodes[recipe_id]

            for fluid_input in recipe_data.fluid_inputs:
                fluid_node_data = disambiguate_fluid(fluid_input)
                if fluid_node_data is None:
                    log.warning(
                        f"Could not disambiguate fluid input: {fluid_input.model_dump_json()} {type(fluid_input)}"
                    )
                    continue

                if fluid_node_data.id not in fluid_nodes:
                    fluid_nodes[fluid_node_data.id] = graph.vcount()
                    graph.add_vertex(name=fluid_node_data.id,
                                     node_type="fluid",
                                     data=fluid_node_data)

                graph.add_edge(fluid_nodes[fluid_node_data.id], recipe_vertex)

            for fluid_output in recipe_data.fluid_outputs:
                fluid_node_data = disambiguate_fluid(fluid_output)
                if fluid_node_data is None:
                    log.warning(
                        f"Could not disambiguate fluid output: {fluid_output.model_dump_json()} {type(fluid_output)}"
                    )
                    continue

                if fluid_node_data.id not in fluid_nodes:
                    fluid_nodes[fluid_node_data.id] = graph.vcount()
                    graph.add_vertex(name=fluid_node_data.id,
                                     node_type="fluid",
                                     data=fluid_node_data)

                graph.add_edge(recipe_vertex, fluid_nodes[fluid_node_data.id])

            for item_input in recipe_data.item_inputs:
                item_node_data = disambiguate_item(item_input)
                if item_node_data is None:
                    log.warning(
                        f"Could not disambiguate item input: {item_input.model_dump_json()} {type(item_input)}"
                    )
                    continue

                if item_node_data.id not in item_nodes:
                    item_nodes[item_node_data.id] = graph.vcount()
                    graph.add_vertex(name=item_node_data.id,
                                     node_type="item",
                                     data=item_node_data)

                graph.add_edge(item_nodes[item_node_data.id], recipe_vertex)

            for item_output in recipe_data.item_outputs:
                item_node_data = disambiguate_item(item_output)
                if item_node_data is None:
                    log.warning(
                        f"Could not disambiguate item output: {item_output.model_dump_json()} {type(item_output)}"
                    )
                    continue

                if item_node_data.id not in item_nodes:
                    item_nodes[item_node_data.id] = graph.vcount()
                    graph.add_vertex(name=item_node_data.id,
                                     node_type="item",
                                     data=item_node_data)

                graph.add_edge(recipe_vertex, item_nodes[item_node_data.id])

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
    recipe_predicate: Optional[Callable[[RecipeNodeData], bool]] = None,
    input_predicate: Optional[Callable[[Union[ItemNodeData, FluidNodeData]],
                                       bool]] = None,
    output_predicate: Optional[Callable[[Union[ItemNodeData, FluidNodeData]],
                                        bool]] = None
) -> ig.Graph:
    vertices_to_keep = set()

    for v in graph.vs:
        if v["node_type"] == "recipe":
            recipe_data = v["data"]
            keep_recipe = True

            if recipe_predicate and not recipe_predicate(recipe_data):
                keep_recipe = False

            if keep_recipe and input_predicate:
                has_matching_input = False
                for predecessor in graph.predecessors(v.index):
                    pred_vertex = graph.vs[predecessor]
                    if input_predicate(pred_vertex["data"]):
                        has_matching_input = True
                        break
                if not has_matching_input:
                    keep_recipe = False

            if keep_recipe and output_predicate:
                has_matching_output = False
                for successor in graph.successors(v.index):
                    succ_vertex = graph.vs[successor]
                    if output_predicate(succ_vertex["data"]):
                        has_matching_output = True
                        break
                if not has_matching_output:
                    keep_recipe = False

            if keep_recipe:
                vertices_to_keep.add(v.index)
                vertices_to_keep.update(graph.predecessors(v.index))
                vertices_to_keep.update(graph.successors(v.index))

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


if __name__ == "__main__":
    content = json.loads(
        Path(
            "/home/haxscramper/.local/share/multimc/instances/1.21.1 V2/.minecraft/kubejs/server_scripts/all_recipes.json"
        ).read_text())

    collection = dict(recipes=content)
    model = parse_recipe_collection(collection)

    Path("/tmp/model.json").write_text(model.model_dump_json(indent=2))
    # exit()

    @beartype
    def disambiguate_fluid(model: FluidModel) -> Optional[FluidNodeData]:
        if model.fluid:
            return FluidNodeData(id=model.fluid)

        elif model.tag:
            return FluidNodeData(id=model.tag)

        return None

    @beartype
    def disambiguate_item(model: ItemModel) -> Optional[ItemNodeData]:
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

    graph = create_recipe_graph(
        model,
        disambiguate_fluid=disambiguate_fluid,
        disambiguate_item=disambiguate_item,
    )
