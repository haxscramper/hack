#!/usr/bin/env python

from typing import Any, Dict, List, Optional, Union
from enum import Enum
from pydantic import BaseModel, Field, model_validator, model_serializer
from dataclasses import dataclass
from beartype.typing import List, Tuple, Optional
from beartype import beartype
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch
from matplotlib.path import Path as MPath
import numpy as np
from plumbum import local
from pathlib import Path
from tempfile import TemporaryDirectory
from log_writer import log


class ELKAlgorithm(str, Enum):
    LAYERED = "layered"
    FORCE = "force"
    STRESS = "stress"
    MRTREE = "mrtree"
    RADIAL = "radial"
    DISCO = "disco"
    RECTPACKING = "rectpacking"


class ELKDirection(str, Enum):
    RIGHT = "RIGHT"
    LEFT = "LEFT"
    DOWN = "DOWN"
    UP = "UP"


class ELKAlignment(str, Enum):
    RIGHT = "RIGHT"
    LEFT = "LEFT"
    CENTER = "CENTER"
    TOP = "TOP"
    BOTTOM = "BOTTOM"


class ELKEdgeRouting(str, Enum):
    ORTHOGONAL = "ORTHOGONAL"
    POLYLINE = "POLYLINE"
    SPLINES = "SPLINES"


class ELKHierarchyHandling(str, Enum):
    SEPARATE_CHILDREN = "SEPARATE_CHILDREN"
    INCLUDE_CHILDREN = "INCLUDE_CHILDREN"


class NodeFlexibility(str, Enum):
    NODE_SIZE = "NODE_SIZE"
    PORT_POSITION = "PORT_POSITION"
    NONE = "NONE"


class PortSide(str, Enum):
    NORTH = "NORTH"
    SOUTH = "SOUTH"
    EAST = "EAST"
    WEST = "WEST"


class PortConstraints(str, Enum):
    FIXED_ORDER = "FIXED_ORDER"
    FIXED_SIDE = "FIXED_SIDE"
    FIXED_POS = "FIXED_POS"
    FREE = "FREE"
    UNDEFINED = "UNDEFINED"


class PortAlignment(str, Enum):
    CENTER = "CENTER"
    BEGIN = "BEGIN"
    END = "END"
    DISTRIBUTED = "DISTRIBUTED"
    JUSTIFIED = "JUSTIFIED"


class ELKNodePlacementBK(BaseModel, extra="forbid"):
    fixedAlignment: Optional[str] = None


class ELKNodePlacement(BaseModel, extra="forbid"):
    bk: Optional[ELKNodePlacementBK] = None


class DirectionCongruency(str, Enum):
    READING_DIRECTION = "READING_DIRECTION"
    ROTATION = "ROTATION"


class InteractiveReferencePoint(str, Enum):
    CENTER = "CENTER"
    TOP_LEFT = "TOP_LEFT"


class PortSortingStrategy(str, Enum):
    INPUT_ORDER = "INPUT_ORDER"
    PORT_DEGREE = "PORT_DEGREE"


class CycleBreakingStrategy(str, Enum):
    GREEDY = "GREEDY"
    DEPTH_FIRST = "DEPTH_FIRST"
    INTERACTIVE = "INTERACTIVE"


class LayeringStrategy(str, Enum):
    NETWORK_SIMPLEX = "NETWORK_SIMPLEX"
    LONGEST_PATH = "LONGEST_PATH"
    COFFMAN_GRAHAM = "COFFMAN_GRAHAM"
    INTERACTIVE = "INTERACTIVE"
    STRETCH_WIDTH = "STRETCH_WIDTH"
    MIN_WIDTH = "MIN_WIDTH"


class LayerConstraint(str, Enum):
    NONE = "NONE"
    FIRST = "FIRST"
    FIRST_SEPARATE = "FIRST_SEPARATE"
    LAST = "LAST"
    LAST_SEPARATE = "LAST_SEPARATE"


class NodePromotionStrategy(str, Enum):
    NONE = "NONE"
    LEFT = "LEFT"
    RIGHT = "RIGHT"
    ALL = "ALL"
    NIKOLOV = "NIKOLOV"


class CrossingMinimizationStrategy(str, Enum):
    LAYER_SWEEP = "LAYER_SWEEP"
    INTERACTIVE = "INTERACTIVE"


class GreedySwitchType(str, Enum):
    OFF = "OFF"
    ONE_SIDED = "ONE_SIDED"
    TWO_SIDED = "TWO_SIDED"


class NodePlacementStrategy(str, Enum):
    BRANDES_KOEPF = "BRANDES_KOEPF"
    LINEAR_SEGMENTS = "LINEAR_SEGMENTS"
    INTERACTIVE = "INTERACTIVE"
    SIMPLE = "SIMPLE"
    NETWORK_SIMPLEX = "NETWORK_SIMPLEX"


class EdgeStraighteningStrategy(str, Enum):
    NONE = "NONE"
    IMPROVE_STRAIGHTNESS = "IMPROVE_STRAIGHTNESS"


class FixedAlignment(str, Enum):
    NONE = "NONE"
    LEFTUP = "LEFTUP"
    RIGHTUP = "RIGHTUP"
    LEFTDOWN = "LEFTDOWN"
    RIGHTDOWN = "RIGHTDOWN"
    BALANCED = "BALANCED"


class NodeFlexibility(str, Enum):
    NONE = "NONE"
    NODE_SIZE = "NODE_SIZE"
    PORT_POSITION = "PORT_POSITION"


class SelfLoopDistributionStrategy(str, Enum):
    NORTH = "NORTH"
    EQUALLY = "EQUALLY"
    NORTH_SOUTH = "NORTH_SOUTH"


class SelfLoopOrderingStrategy(str, Enum):
    STACKED = "STACKED"
    SEQUENCED = "SEQUENCED"


class SplineRoutingMode(str, Enum):
    SLOPPY = "SLOPPY"
    CONSERVATIVE = "CONSERVATIVE"


class ELKLayeredCycleBreaking(BaseModel, extra="forbid"):
    strategy: Optional[CycleBreakingStrategy] = Field(
        None,
        description=
        "Strategy for cycle breaking. Cycle breaking looks for cycles in the graph and determines which edges to reverse to break the cycles."
    )


class ELKLayeredLayering(BaseModel, extra="forbid"):
    strategy: Optional[LayeringStrategy] = Field(
        None, description="Strategy for node layering.")
    layerConstraint: Optional[LayerConstraint] = Field(
        None,
        description=
        "Determines a constraint on the placement of the node regarding the layering."
    )
    layerChoiceConstraint: Optional[int] = Field(
        None,
        ge=-1,
        description=
        "Allows to set a constraint regarding the layer placement of a node.")
    layerId: Optional[int] = Field(
        None,
        ge=-1,
        description=
        "Layer identifier that was calculated by ELK Layered for a node.")


class ELKLayeredLayeringMinWidth(BaseModel, extra="forbid"):
    upperBoundOnWidth: Optional[int] = Field(
        None,
        ge=-1,
        description=
        "Defines a loose upper bound on the width of the MinWidth layerer.")
    upperLayerEstimationScalingFactor: Optional[int] = Field(
        None,
        ge=-1,
        description=
        "Multiplied with Upper Bound On Width for defining an upper bound on the width of layers."
    )


class ELKLayeredLayeringNodePromotion(BaseModel, extra="forbid"):
    strategy: Optional[NodePromotionStrategy] = Field(
        None,
        description=
        "Reduces number of dummy nodes after layering phase (if possible).")
    maxIterations: Optional[int] = Field(
        None,
        ge=0,
        description="Limits the number of iterations for node promotion.")


class ELKLayeredLayeringCoffmanGraham(BaseModel, extra="forbid"):
    layerBound: Optional[int] = Field(
        None, description="The maximum number of nodes allowed per layer.")


class ELKLayeredCrossingMinimization(BaseModel, extra="forbid"):
    strategy: Optional[CrossingMinimizationStrategy] = Field(
        None, description="Strategy for crossing minimization.")
    forceNodeModelOrder: Optional[bool] = Field(
        None,
        description=
        "The node order given by the model does not change to produce a better layout."
    )
    hierarchicalSweepiness: Optional[float] = Field(
        None,
        description=
        "How likely it is to use cross-hierarchy (1) vs bottom-up (-1).")
    semiInteractive: Optional[bool] = Field(
        None,
        description=
        "Preserves the order of nodes within a layer but still minimizes crossings between edges."
    )
    inLayerPredOf: Optional[str] = Field(
        None,
        description=
        "Specifies of which node the current node is the predecessor.")
    inLayerSuccOf: Optional[str] = Field(
        None,
        description="Specifies of which node the current node is the successor."
    )
    positionChoiceConstraint: Optional[int] = Field(
        None,
        ge=-1,
        description=
        "Allows to set a constraint regarding the position placement of a node in a layer."
    )
    positionId: Optional[int] = Field(
        None,
        ge=-1,
        description=
        "Position within a layer that was determined by ELK Layered for a node."
    )


class ELKLayeredCrossingMinimizationGreedySwitch(BaseModel, extra="forbid"):
    activationThreshold: Optional[int] = Field(
        None,
        ge=0,
        description="Threshold for automatic activation of greedy switch.")
    type: Optional[GreedySwitchType] = Field(
        None, description="Greedy Switch strategy for crossing minimization.")


class ELKLayeredCrossingMinimizationGreedySwitchHierarchical(BaseModel,
                                                             extra="forbid"):
    type: Optional[GreedySwitchType] = Field(
        None,
        description=
        "Activates the greedy switch heuristic in case hierarchical layout is used."
    )


class ELKLayeredNodePlacementBK(BaseModel, extra="forbid"):
    edgeStraightening: Optional[EdgeStraighteningStrategy] = Field(
        None,
        description=
        "Specifies whether the Brandes Koepf node placer tries to increase the number of straight edges."
    )
    fixedAlignment: Optional[FixedAlignment] = Field(
        None,
        description=
        "Tells the BK node placer to use a certain alignment instead of the one producing the smallest height."
    )


class ELKLayeredNodePlacementLinearSegments(BaseModel, extra="forbid"):
    deflectionDampening: Optional[float] = Field(
        None,
        gt=0,
        description=
        "Dampens the movement of nodes to keep the diagram from getting too large."
    )


class ELKLayeredNodePlacementNetworkSimplex(BaseModel, extra="forbid"):
    nodeFlexibility: Optional[NodeFlexibility] = Field(
        None, description="Aims at shorter and straighter edges.")


class ELKLayeredNodePlacementNetworkSimplexNodeFlexibility(BaseModel,
                                                           extra="forbid"):
    default: Optional[NodeFlexibility] = Field(
        None,
        description=
        "Default value of the 'nodeFlexibility' option for the children of a hierarchical node."
    )


class ELKLayeredNodePlacement(BaseModel, extra="forbid"):
    strategy: Optional[NodePlacementStrategy] = Field(
        None, description="Strategy for node placement.")
    favorStraightEdges: Optional[bool] = Field(
        None,
        description="Favor straight edges over a balanced node placement.")
    bk: Optional[ELKLayeredNodePlacementBK] = None
    linearSegments: Optional[ELKLayeredNodePlacementLinearSegments] = None
    networkSimplex: Optional[ELKLayeredNodePlacementNetworkSimplex] = None


class ELKLayeredEdgeRoutingSplines(BaseModel, extra="forbid"):
    mode: Optional[SplineRoutingMode] = Field(
        None,
        description=
        "Specifies the way control points are assembled for each individual edge."
    )
    sloppyLayerSpacingFactor: Optional[float] = Field(
        None, description="Factor for sloppy spline layer spacing.")


class ELKLayeredEdgeRouting(BaseModel, extra="forbid"):
    selfLoopDistribution: Optional[SelfLoopDistributionStrategy] = Field(
        None,
        description="Alter the distribution of the loops around the node.")
    selfLoopOrdering: Optional[SelfLoopOrderingStrategy] = Field(
        None,
        description=
        "Alter the ordering of the loops they can either be stacked or sequenced."
    )
    splines: Optional[ELKLayeredEdgeRoutingSplines] = None


class ELKLayered(BaseModel, extra="forbid"):
    directionCongruency: Optional[DirectionCongruency] = Field(
        None,
        description=
        "Specifies how drawings of the same graph with different layout directions compare to each other."
    )
    feedbackEdges: Optional[bool] = Field(
        None,
        description=
        "Whether feedback edges should be highlighted by routing around the nodes."
    )
    interactiveReferencePoint: Optional[InteractiveReferencePoint] = Field(
        None,
        description=
        "Determines which point of a node is considered by interactive layout phases."
    )
    mergeEdges: Optional[bool] = Field(
        None,
        description=
        "Edges that have no ports are merged so they touch the connected nodes at the same points."
    )
    mergeHierarchyEdges: Optional[bool] = Field(
        None,
        description=
        "If hierarchical layout is active, hierarchy-crossing edges use as few hierarchical ports as possible."
    )
    allowNonFlowPortsToSwitchSides: Optional[bool] = Field(
        None,
        description=
        "Specifies whether non-flow ports may switch sides if their node's port constraints are either FIXED_SIDE or FIXED_ORDER."
    )
    portSortingStrategy: Optional[PortSortingStrategy] = Field(
        None,
        description=
        "Determines the way a node's ports are distributed on the sides of a node if their order is not prescribed."
    )
    thoroughness: Optional[int] = Field(
        None,
        ge=1,
        description="How much effort should be spent to produce a nice layout."
    )
    unnecessaryBendpoints: Optional[bool] = Field(
        None,
        description=
        "Adds bend points even if an edge does not change direction.")
    generatePositionAndLayerIds: Optional[bool] = Field(
        None, description="If enabled position id and layer id are generated.")
    cycleBreaking: Optional[ELKLayeredCycleBreaking] = None
    layering: Optional[ELKLayeredLayering] = None
    crossingMinimization: Optional[ELKLayeredCrossingMinimization] = None
    nodePlacement: Optional[ELKLayeredNodePlacement] = None
    edgeRouting: Optional[ELKLayeredEdgeRouting] = None


class ELKSpacing(BaseModel, extra="forbid"):
    edgeNode: Optional[float] = None
    nodeNode: Optional[float] = None
    edgeEdge: Optional[float] = None
    edgeLabel: Optional[float] = None
    labelLabel: Optional[float] = None
    labelNode: Optional[float] = None
    labelPort: Optional[float] = None
    portPort: Optional[float] = None
    componentComponent: Optional[float] = None


class LayoutOptionsELK(BaseModel, extra="forbid"):
    algorithm: Optional[ELKAlgorithm] = None
    layered: Optional[ELKLayered] = None
    hierarchyHandling: Optional[ELKHierarchyHandling] = None
    alignment: Optional[ELKAlignment] = None
    direction: Optional[ELKDirection] = None
    aspectRatio: Optional[float] = None
    edgeRouting: Optional[ELKEdgeRouting] = None
    spacing: Optional[ELKSpacing] = None


class LayoutOptionsPartitioning(BaseModel, extra="forbid"):
    activate: Optional[bool] = None


class LayoutOptionsBase(BaseModel, extra="forbid"):

    @model_validator(mode="before")
    @classmethod
    def unflatten_options(cls, values: Dict[str, Any]) -> Dict[str, Any]:
        if not isinstance(values, dict):
            return values

        result = {}
        nested_data = {}

        for key, value in values.items():
            if "." in key:
                parts = key.split(".")
                current = nested_data

                for part in parts[:-1]:
                    if part not in current:
                        current[part] = {}
                    current = current[part]

                current[parts[-1]] = value
            else:
                result[key] = value

        result.update(nested_data)
        return result

    def flatten_for_export(self, use_dotted: bool = True) -> Dict[str, Any]:
        if not use_dotted:
            return self.model_dump(exclude_none=True)

        data = self.model_dump(exclude_none=True)

        def flatten_dict(d: Dict[str, Any],
                         parent_key: str = "") -> Dict[str, Any]:
            items = []
            for k, v in d.items():
                new_key = f"{parent_key}.{k}" if parent_key else k
                if isinstance(v, dict):
                    items.extend(flatten_dict(v, new_key).items())
                else:
                    items.append((new_key, v))
            return dict(items)

        result = {}
        for key, value in data.items():
            if isinstance(value, dict):
                flattened = flatten_dict(value, key)
                result.update(flattened)
            else:
                result[key] = value

        return result


class GraphLayoutOptions(LayoutOptionsBase, extra="forbid"):
    elk: Optional[LayoutOptionsELK] = None
    partitioning: Optional[LayoutOptionsPartitioning] = None
    nodeFlexibility: Optional[NodeFlexibility] = None


class PortProperties(BaseModel, extra="forbid"):
    port: Optional[Dict[str, Any]] = None
    portConstraints: Optional[PortConstraints] = None
    portAlignment: Optional[PortAlignment] = None
    allowNonFlowPortsToSwitchSides: Optional[bool] = None
    side: Optional[PortSide] = None

    @model_validator(mode="before")
    @classmethod
    def unflatten_port_properties(cls, values: Dict[str,
                                                    Any]) -> Dict[str, Any]:
        if not isinstance(values, dict):
            return values

        result = {}
        port_data = {}

        for key, value in values.items():
            if key.startswith("port."):
                parts = key.split(".", 1)
                if len(parts) == 2:
                    if "port" not in port_data:
                        port_data["port"] = {}
                    port_data["port"][parts[1]] = value
            else:
                result[key] = value

        result.update(port_data)
        return result

    @model_serializer(mode='plain')
    def serialize_model(self) -> Dict[str, Any]:
        data = {}

        if self.portConstraints is not None:
            data["portConstraints"] = self.portConstraints
        if self.portAlignment is not None:
            data["portAlignment"] = self.portAlignment
        if self.allowNonFlowPortsToSwitchSides is not None:
            data[
                "allowNonFlowPortsToSwitchSides"] = self.allowNonFlowPortsToSwitchSides

        # Handle port nested data
        if hasattr(self, "__pydantic_extra__") and self.__pydantic_extra__:
            for key, value in self.__pydantic_extra__.items():
                if key == "port" and isinstance(value, dict):
                    for port_key, port_value in value.items():
                        data[f"port.{port_key}"] = port_value
                else:
                    data[key] = value

        return data


class NodeProperties(BaseModel, extra="forbid"):
    portConstraints: Optional[PortConstraints] = None
    portAlignment: Optional[PortAlignment] = None


class Point(BaseModel, extra="forbid"):
    x: float
    y: float


class LabelLayoutOptions(BaseModel, extra="forbid"):
    pass


class Label(BaseModel, extra="forbid"):
    id: Optional[Union[str, int]] = None
    x: Optional[float] = None
    y: Optional[float] = None
    width: Optional[float] = None
    height: Optional[float] = None
    text: Optional[str] = None
    labels: Optional[List["Label"]] = None
    layoutOptions: Optional[LabelLayoutOptions] = None


class PortLayoutOptions(LayoutOptionsBase, extra="forbid"):
    pass


class Port(BaseModel, extra="forbid"):
    id: Union[str, int]
    x: Optional[float] = None
    y: Optional[float] = None
    width: Optional[float] = None
    height: Optional[float] = None
    labels: Optional[List[Label]] = None
    properties: Optional[PortProperties] = None
    layoutOptions: Optional[PortLayoutOptions] = None


class EdgeSection(BaseModel, extra="forbid"):
    id: Optional[Union[str, int]] = None
    startPoint: Point
    endPoint: Point
    bendPoints: Optional[List[Point]] = None
    incomingShape: Optional[Union[str, int]] = None
    outgoingShape: Optional[Union[str, int]] = None
    incomingSections: Optional[List[Union[str, int]]] = None
    outgoingSections: Optional[List[Union[str, int]]] = None


class EdgeLayoutOptions(LayoutOptionsBase, extra="forbid"):
    junctionPoints: Optional[List[Point]] = None


class Edge(BaseModel, extra="forbid"):
    id: Union[str, int]
    source: Optional[Union[str, int]] = None
    sourcePort: Optional[Union[str, int]] = None
    target: Optional[Union[str, int]] = None
    targetPort: Optional[Union[str, int]] = None
    sources: Optional[List[Union[str, int]]] = None
    targets: Optional[List[Union[str, int]]] = None
    sourcePoint: Optional[Point] = None
    targetPoint: Optional[Point] = None
    bendPoints: Optional[List[Point]] = None
    sections: Optional[List[EdgeSection]] = None
    labels: Optional[List[Label]] = None
    junctionPoints: Optional[List[Point]] = None
    layoutOptions: Optional[EdgeLayoutOptions] = None


class NodeLayoutOptions(LayoutOptionsBase, extra="forbid"):
    pass


class Node(BaseModel, extra="forbid"):
    id: Union[str, int]
    x: Optional[float] = None
    y: Optional[float] = None
    width: Optional[float] = None
    height: Optional[float] = None
    type: Optional[str] = None
    ports: Optional[List[Port]] = None
    labels: Optional[List[Label]] = None
    children: Optional[List["Node"]] = None
    edges: Optional[List[Edge]] = None
    properties: Optional[NodeProperties] = None
    layoutOptions: Optional[NodeLayoutOptions] = None


class Graph(BaseModel, extra="forbid"):
    id: Union[str, int]
    x: Optional[float] = None
    y: Optional[float] = None
    width: Optional[float] = None
    height: Optional[float] = None
    layoutOptions: Optional[GraphLayoutOptions] = None
    children: List[Node] = Field(default_factory=list)
    edges: Optional[List[Edge]] = None
    ports: Optional[List[Port]] = None
    labels: Optional[List[Label]] = None


Node.model_rebuild()
Label.model_rebuild()

import json
from pathlib import Path
from typing import Any, Dict, Optional


class GraphSerializer:

    @staticmethod
    def to_dotted_json(graph: Graph) -> Dict[str, Any]:
        data = graph.model_dump(exclude_none=True)

        if "layoutOptions" in data and isinstance(data["layoutOptions"], dict):
            layout_options = LayoutOptions(**data["layoutOptions"])
            data["layoutOptions"] = layout_options.flatten_for_export(
                use_dotted=True)

        def process_node(node: Dict[str, Any]) -> Dict[str, Any]:
            if "layoutOptions" in node and isinstance(node["layoutOptions"],
                                                      dict):
                layout_options = LayoutOptions(**node["layoutOptions"])
                node["layoutOptions"] = layout_options.flatten_for_export(
                    use_dotted=True)

            if "children" in node:
                node["children"] = [
                    process_node(child) for child in node["children"]
                ]

            return node

        if "children" in data:
            data["children"] = [
                process_node(child) for child in data["children"]
            ]

        return data

    @staticmethod
    def to_nested_json(graph: Graph) -> Dict[str, Any]:
        return graph.model_dump(exclude_none=True)

    @staticmethod
    def from_json(data: Dict[str, Any]) -> Graph:
        return Graph(**data)

    @staticmethod
    def save_to_file(graph: Graph,
                     filepath: Path,
                     use_dotted: bool = True) -> None:
        if use_dotted:
            data = GraphSerializer.to_dotted_json(graph)
        else:
            data = GraphSerializer.to_nested_json(graph)

        with open(filepath, "w") as f:
            json.dump(data, f, indent=2)

    @staticmethod
    def load_from_file(filepath: Path) -> Graph:
        with open(filepath, "r") as f:
            data = json.load(f)
        return GraphSerializer.from_json(data)


def validate_graph_structure(graph: Graph) -> bool:
    node_ids = set()
    port_ids = set()
    edge_ids = set()

    def collect_node_ids(node: Node) -> None:
        if node.id in node_ids:
            raise ValueError(f"Duplicate node id: {node.id}")
        node_ids.add(node.id)

        if node.ports:
            for port in node.ports:
                if port.id in port_ids:
                    raise ValueError(f"Duplicate port id: {port.id}")
                port_ids.add(port.id)

        if node.children:
            for child in node.children:
                collect_node_ids(child)

    if graph.children:
        for node in graph.children:
            collect_node_ids(node)

    if graph.ports:
        for port in graph.ports:
            if port.id in port_ids:
                raise ValueError(f"Duplicate port id: {port.id}")
            port_ids.add(port.id)

    def validate_edges(edges: Optional[List[Edge]]) -> None:
        if not edges:
            return

        for edge in edges:
            if edge.id in edge_ids:
                raise ValueError(f"Duplicate edge id: {edge.id}")
            edge_ids.add(edge.id)

            if edge.source is not None:
                if edge.source not in node_ids and edge.source not in port_ids:
                    raise ValueError(
                        f"Edge {edge.id} references unknown source: {edge.source}"
                    )

            if edge.target is not None:
                if edge.target not in node_ids and edge.target not in port_ids:
                    raise ValueError(
                        f"Edge {edge.id} references unknown target: {edge.target}"
                    )

            if edge.sourcePort is not None:
                if edge.sourcePort not in port_ids:
                    raise ValueError(
                        f"Edge {edge.id} references unknown source port: {edge.sourcePort}"
                    )

            if edge.targetPort is not None:
                if edge.targetPort not in port_ids:
                    raise ValueError(
                        f"Edge {edge.id} references unknown target port: {edge.targetPort}"
                    )

            if edge.sources:
                for source in edge.sources:
                    if source not in node_ids and source not in port_ids:
                        raise ValueError(
                            f"Edge {edge.id} references unknown source: {source}"
                        )

            if edge.targets:
                for target in edge.targets:
                    if target not in node_ids and target not in port_ids:
                        raise ValueError(
                            f"Edge {edge.id} references unknown target: {target}"
                        )

    validate_edges(graph.edges)

    def validate_node_edges(node: Node) -> None:
        if node.edges:
            validate_edges(node.edges)
        if node.children:
            for child in node.children:
                validate_node_edges(child)

    if graph.children:
        for node in graph.children:
            validate_node_edges(node)

    return True


@dataclass
class AbsolutePosition:
    x: float
    y: float
    width: float
    height: float
    id: str
    type: Optional[str] = None


@beartype
def compute_absolute_positions(
    graph: Graph
) -> Tuple[List[AbsolutePosition], List[Tuple[Point, List[Point], Point]]]:
    nodes = []
    edges_coords = []

    def process_node(node: Node,
                     parent_x: float = 0,
                     parent_y: float = 0) -> None:
        abs_x = parent_x + (node.x or 0)
        abs_y = parent_y + (node.y or 0)

        nodes.append(
            AbsolutePosition(x=abs_x,
                             y=abs_y,
                             width=node.width or 0,
                             height=node.height or 0,
                             id=str(node.id),
                             type=node.type))

        if node.children:
            for child in node.children:
                process_node(child, abs_x, abs_y)

    if graph.children:
        for node in graph.children:
            process_node(node)

    def process_edges(edges: Optional[List[Edge]],
                      container_x: float = 0,
                      container_y: float = 0) -> None:
        if not edges:
            return

        for edge in edges:
            if edge.sections:
                for section in edge.sections:
                    start = Point(x=container_x + section.startPoint.x,
                                  y=container_y + section.startPoint.y)
                    end = Point(x=container_x + section.endPoint.x,
                                y=container_y + section.endPoint.y)
                    bends = []
                    if section.bendPoints:
                        for bend in section.bendPoints:
                            bends.append(
                                Point(x=container_x + bend.x,
                                      y=container_y + bend.y))
                    edges_coords.append((start, bends, end))
            elif edge.sourcePoint and edge.targetPoint:
                start = Point(x=container_x + edge.sourcePoint.x,
                              y=container_y + edge.sourcePoint.y)
                end = Point(x=container_x + edge.targetPoint.x,
                            y=container_y + edge.targetPoint.y)
                bends = []
                if edge.bendPoints:
                    for bend in edge.bendPoints:
                        bends.append(
                            Point(x=container_x + bend.x,
                                  y=container_y + bend.y))
                edges_coords.append((start, bends, end))

    process_edges(graph.edges)

    def process_node_edges(node: Node,
                           parent_x: float = 0,
                           parent_y: float = 0) -> None:
        abs_x = parent_x + (node.x or 0)
        abs_y = parent_y + (node.y or 0)

        process_edges(node.edges, abs_x, abs_y)

        if node.children:
            for child in node.children:
                process_node_edges(child, abs_x, abs_y)

    if graph.children:
        for node in graph.children:
            process_node_edges(node)

    return nodes, edges_coords


def render_to_png(nodes: List[AbsolutePosition],
                  edges: List[Tuple[Point, List[Point],
                                    Point]], output_file: Path) -> None:
    fig, ax = plt.subplots(1, 1, figsize=(16, 10))

    max_x = max((n.x + n.width for n in nodes), default=100)
    max_y = max((n.y + n.height for n in nodes), default=100)

    ax.set_xlim(-10, max_x + 10)
    ax.set_ylim(-10, max_y + 10)
    ax.invert_yaxis()
    ax.set_aspect("equal")
    ax.axis("off")

    node_colors = {
        "start": "#90EE90",
        "activity": "#87CEEB",
        "gateway": "#FFD700",
        None: "#D3D3D3"
    }

    for node in nodes:
        color = node_colors.get(node.type, "#D3D3D3")

        if node.type == "gateway":
            diamond = patches.FancyBboxPatch((node.x, node.y),
                                             node.width,
                                             node.height,
                                             boxstyle="round,pad=0.02",
                                             facecolor=color,
                                             edgecolor="black",
                                             linewidth=1.5)
            ax.add_patch(diamond)
        elif node.type == "start":
            circle = patches.Circle(
                (node.x + node.width / 2, node.y + node.height / 2),
                min(node.width, node.height) / 2,
                facecolor=color,
                edgecolor="black",
                linewidth=1.5)
            ax.add_patch(circle)
        else:
            rect = patches.Rectangle((node.x, node.y),
                                     node.width,
                                     node.height,
                                     facecolor=color,
                                     edgecolor="black",
                                     linewidth=1.5)
            ax.add_patch(rect)

        ax.text(node.x + node.width / 2,
                node.y + node.height / 2,
                node.id,
                ha="center",
                va="center",
                fontsize=8,
                fontweight="bold")

    for start, bends, end in edges:
        points = [start] + bends + [end]

        for i in range(len(points) - 1):
            ax.plot([points[i].x, points[i + 1].x],
                    [points[i].y, points[i + 1].y],
                    "k-",
                    linewidth=1.5)

        if len(points) < 2:
            continue

        last_segment_start = points[-2]
        last_segment_end = points[-1]

        dx = last_segment_end.x - last_segment_start.x
        dy = last_segment_end.y - last_segment_start.y
        length = np.sqrt(dx**2 + dy**2)

        if length < 0.001:
            continue

        arrow = FancyArrowPatch((last_segment_start.x, last_segment_start.y),
                                (last_segment_end.x, last_segment_end.y),
                                arrowstyle="->",
                                mutation_scale=15,
                                linewidth=1.5,
                                color="black")
        ax.add_patch(arrow)

    plt.tight_layout()
    plt.savefig(output_file, dpi=150, bbox_inches="tight")
    plt.close()

    log.info(f"Graph rendered to {output_file}")


script_dir = Path(
    "/home/haxscramper/workspace/repos/hack/testing/java/elk-graph-layout-kotlin"
)
script_path = script_dir / "scripts" / "run.sh"


def perform_graph_layout(graph: Graph) -> Graph:
    if graph.layoutOptions and graph.layoutOptions.elk:
        if graph.layoutOptions.elk.algorithm:
            log.info(f"  Algorithm: {graph.layoutOptions.elk.algorithm}")
        if graph.layoutOptions.elk.direction:
            log.info(f"  Direction: {graph.layoutOptions.elk.direction}")

    validate_graph_structure(graph)

    with TemporaryDirectory() as output_subdir:
        dir = Path(output_subdir)
        dir = Path("/tmp")
        validated_path = dir / f"result_validated.json"
        GraphSerializer.save_to_file(graph, validated_path, use_dotted=True)

        layout_path = dir / f"result_layout.json"

        cmd = local[str(script_path)].with_cwd(script_dir)
        cmd.run([f"--input={validated_path}", f"--output={layout_path}"])

        return GraphSerializer.load_from_file(layout_path)


def process_graph_files():
    input_dir = Path("elk-models")
    output_dir = Path("/tmp/elk-layout")

    for json_file in input_dir.rglob("*.json"):
        relative_path = json_file.relative_to(input_dir)
        output_subdir = output_dir / relative_path.parent
        output_subdir.mkdir(parents=True, exist_ok=True)

        base_name = relative_path.stem

        log.info(f"Processing {json_file}")

        sample_data = json.loads(json_file.read_text())
        graph = Graph(**sample_data)

        if graph.layoutOptions and graph.layoutOptions.elk:
            if graph.layoutOptions.elk.algorithm:
                log.info(f"  Algorithm: {graph.layoutOptions.elk.algorithm}")
            if graph.layoutOptions.elk.direction:
                log.info(f"  Direction: {graph.layoutOptions.elk.direction}")

        validate_graph_structure(graph)

        validated_path = output_subdir / f"{base_name}_validated.json"
        GraphSerializer.save_to_file(graph, validated_path, use_dotted=True)

        layout_path = output_subdir / f"{base_name}_layout.json"

        cmd = local[str(script_path)].with_cwd(script_dir)
        cmd.run([f"--input={validated_path}", f"--output={layout_path}"])

        graph = GraphSerializer.load_from_file(layout_path)

        nodes, edges = compute_absolute_positions(graph)
        png_path = output_subdir / f"{base_name}.png"
        render_to_png(nodes, edges, png_path)

        log.info(f"  Completed: {png_path}")


if __name__ == "__main__":
    log.basicConfig(
        level=log.DEBUG,
        format=
        "%(asctime)s %(name)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s"
    )

    log.getLogger("matplotlib.font_manager").setLevel(log.INFO)

    process_graph_files()
