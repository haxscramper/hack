#!/usr/bin/env python

from typing import Any, Dict, List, Optional, Union
from enum import Enum
from pydantic import BaseModel, Field, model_validator, model_serializer
from dataclasses import dataclass
from typing import List, Tuple, Optional
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch
from matplotlib.path import Path as MPath
import numpy as np
from plumbum import local
from pathlib import Path


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


class ELKNodePlacementBK(BaseModel):
    fixedAlignment: Optional[str] = None

    class Config:
        extra = "allow"


class ELKNodePlacement(BaseModel):
    bk: Optional[ELKNodePlacementBK] = None

    class Config:
        extra = "allow"


class ELKLayered(BaseModel):
    feedbackEdges: Optional[bool] = None
    nodePlacement: Optional[ELKNodePlacement] = None
    allowNonFlowPortsToSwitchSides: Optional[bool] = None

    class Config:
        extra = "allow"


class ELKSpacing(BaseModel):
    edgeNode: Optional[float] = None
    nodeNode: Optional[float] = None
    edgeEdge: Optional[float] = None
    edgeLabel: Optional[float] = None
    labelLabel: Optional[float] = None
    labelNode: Optional[float] = None
    labelPort: Optional[float] = None
    portPort: Optional[float] = None
    componentComponent: Optional[float] = None

    class Config:
        extra = "allow"


class LayoutOptionsELK(BaseModel):
    algorithm: Optional[ELKAlgorithm] = None
    layered: Optional[ELKLayered] = None
    hierarchyHandling: Optional[ELKHierarchyHandling] = None
    alignment: Optional[ELKAlignment] = None
    direction: Optional[ELKDirection] = None
    aspectRatio: Optional[float] = None
    edgeRouting: Optional[ELKEdgeRouting] = None
    spacing: Optional[ELKSpacing] = None

    class Config:
        extra = "allow"


class LayoutOptionsPartitioning(BaseModel):
    activate: Optional[bool] = None

    class Config:
        extra = "allow"


class LayoutOptions(BaseModel):
    elk: Optional[LayoutOptionsELK] = None
    partitioning: Optional[LayoutOptionsPartitioning] = None
    nodeFlexibility: Optional[NodeFlexibility] = None

    class Config:
        extra = "allow"

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


class PortProperties(BaseModel):
    port: Optional[Dict[str, Any]] = None
    portConstraints: Optional[PortConstraints] = None
    portAlignment: Optional[PortAlignment] = None
    allowNonFlowPortsToSwitchSides: Optional[bool] = None

    class Config:
        extra = "allow"

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
            data["allowNonFlowPortsToSwitchSides"] = self.allowNonFlowPortsToSwitchSides
        
        # Handle port nested data
        if hasattr(self, "__pydantic_extra__") and self.__pydantic_extra__:
            for key, value in self.__pydantic_extra__.items():
                if key == "port" and isinstance(value, dict):
                    for port_key, port_value in value.items():
                        data[f"port.{port_key}"] = port_value
                else:
                    data[key] = value
        
        return data


class NodeProperties(BaseModel):
    portConstraints: Optional[PortConstraints] = None
    portAlignment: Optional[PortAlignment] = None

    class Config:
        extra = "allow"


class Point(BaseModel):
    x: float
    y: float


class Label(BaseModel):
    id: Optional[Union[str, int]] = None
    x: Optional[float] = None
    y: Optional[float] = None
    width: Optional[float] = None
    height: Optional[float] = None
    text: Optional[str] = None
    labels: Optional[List["Label"]] = None
    layoutOptions: Optional[Dict[str, Any]] = None


class Port(BaseModel):
    id: Union[str, int]
    x: Optional[float] = None
    y: Optional[float] = None
    width: Optional[float] = None
    height: Optional[float] = None
    labels: Optional[List[Label]] = None
    properties: Optional[PortProperties] = None
    layoutOptions: Optional[Dict[str, Any]] = None


class EdgeSection(BaseModel):
    id: Optional[Union[str, int]] = None
    startPoint: Point
    endPoint: Point
    bendPoints: Optional[List[Point]] = None
    incomingShape: Optional[Union[str, int]] = None
    outgoingShape: Optional[Union[str, int]] = None
    incomingSections: Optional[List[Union[str, int]]] = None
    outgoingSections: Optional[List[Union[str, int]]] = None


class Edge(BaseModel):
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
    layoutOptions: Optional[Dict[str, Any]] = None


class Node(BaseModel):
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
    layoutOptions: Optional[LayoutOptions] = None


class Graph(BaseModel):
    id: Union[str, int]
    x: Optional[float] = None
    y: Optional[float] = None
    width: Optional[float] = None
    height: Optional[float] = None
    layoutOptions: Optional[LayoutOptions] = None
    children: Optional[List[Node]] = None
    edges: Optional[List[Edge]] = None
    ports: Optional[List[Port]] = None
    labels: Optional[List[Label]] = None


Node.model_rebuild()
Label.model_rebuild()

#!/usr/bin/env python

import json
import logging
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

def compute_absolute_positions(graph: Graph) -> Tuple[List[AbsolutePosition], List[Tuple[Point, List[Point], Point]]]:
    nodes = []
    edges_coords = []
    
    def process_node(node: Node, parent_x: float = 0, parent_y: float = 0) -> None:
        abs_x = parent_x + (node.x or 0)
        abs_y = parent_y + (node.y or 0)
        
        nodes.append(AbsolutePosition(
            x=abs_x,
            y=abs_y,
            width=node.width or 0,
            height=node.height or 0,
            id=str(node.id),
            type=node.type
        ))
        
        if node.children:
            for child in node.children:
                process_node(child, abs_x, abs_y)
    
    if laid_out_graph.children:
        for node in laid_out_graph.children:
            process_node(node)
    
    def process_edges(edges: Optional[List[Edge]], container_x: float = 0, container_y: float = 0) -> None:
        if not edges:
            return
            
        for edge in edges:
            if edge.sections:
                for section in edge.sections:
                    start = Point(
                        x=container_x + section.startPoint.x,
                        y=container_y + section.startPoint.y
                    )
                    end = Point(
                        x=container_x + section.endPoint.x,
                        y=container_y + section.endPoint.y
                    )
                    bends = []
                    if section.bendPoints:
                        for bend in section.bendPoints:
                            bends.append(Point(
                                x=container_x + bend.x,
                                y=container_y + bend.y
                            ))
                    edges_coords.append((start, bends, end))
            elif edge.sourcePoint and edge.targetPoint:
                start = Point(
                    x=container_x + edge.sourcePoint.x,
                    y=container_y + edge.sourcePoint.y
                )
                end = Point(
                    x=container_x + edge.targetPoint.x,
                    y=container_y + edge.targetPoint.y
                )
                bends = []
                if edge.bendPoints:
                    for bend in edge.bendPoints:
                        bends.append(Point(
                            x=container_x + bend.x,
                            y=container_y + bend.y
                        ))
                edges_coords.append((start, bends, end))
    
    process_edges(laid_out_graph.edges)
    
    def process_node_edges(node: Node, parent_x: float = 0, parent_y: float = 0) -> None:
        abs_x = parent_x + (node.x or 0)
        abs_y = parent_y + (node.y or 0)
        
        process_edges(node.edges, abs_x, abs_y)
        
        if node.children:
            for child in node.children:
                process_node_edges(child, abs_x, abs_y)
    
    if laid_out_graph.children:
        for node in laid_out_graph.children:
            process_node_edges(node)
    
    return nodes, edges_coords

def render_to_png(nodes: List[AbsolutePosition], edges: List[Tuple[Point, List[Point], Point]], output_file: Path) -> None:
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
            diamond = patches.FancyBboxPatch(
                (node.x, node.y),
                node.width,
                node.height,
                boxstyle="round,pad=0.02",
                facecolor=color,
                edgecolor="black",
                linewidth=1.5
            )
            ax.add_patch(diamond)
        elif node.type == "start":
            circle = patches.Circle(
                (node.x + node.width/2, node.y + node.height/2),
                min(node.width, node.height)/2,
                facecolor=color,
                edgecolor="black",
                linewidth=1.5
            )
            ax.add_patch(circle)
        else:
            rect = patches.Rectangle(
                (node.x, node.y),
                node.width,
                node.height,
                facecolor=color,
                edgecolor="black",
                linewidth=1.5
            )
            ax.add_patch(rect)
        
        ax.text(
            node.x + node.width/2,
            node.y + node.height/2,
            node.id,
            ha="center",
            va="center",
            fontsize=8,
            fontweight="bold"
        )
    
    for start, bends, end in edges:
        points = [start] + bends + [end]
        
        for i in range(len(points) - 1):
            ax.plot(
                [points[i].x, points[i+1].x],
                [points[i].y, points[i+1].y],
                "k-",
                linewidth=1.5
            )
        
        if len(points) < 2:
            continue
            
        last_segment_start = points[-2]
        last_segment_end = points[-1]
        
        dx = last_segment_end.x - last_segment_start.x
        dy = last_segment_end.y - last_segment_start.y
        length = np.sqrt(dx**2 + dy**2)
        
        if length < 0.001:
            continue
            
        arrow = FancyArrowPatch(
            (last_segment_start.x, last_segment_start.y),
            (last_segment_end.x, last_segment_end.y),
            arrowstyle="->",
            mutation_scale=15,
            linewidth=1.5,
            color="black"
        )
        ax.add_patch(arrow)
    
    plt.tight_layout()
    plt.savefig(output_file, dpi=150, bbox_inches="tight")
    plt.close()
    
    logging.info(f"Graph rendered to {output_file}")



if __name__ == "__main__":
    logging.basicConfig(
        level=logging.DEBUG,
        format=
        "%(asctime)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s")

    sample_data = json.loads(Path("/tmp/input_graph.json").read_text())

    graph = Graph(**sample_data)

    if graph.layoutOptions:
        logging.info(f"Algorithm: {graph.layoutOptions.elk.algorithm}")
        logging.info(f"Direction: {graph.layoutOptions.elk.direction}")
        logging.info(f"Edge spacing: {graph.layoutOptions.elk.spacing.edgeNode}")
        logging.info(f"Node flexibility: {graph.layoutOptions.nodeFlexibility}")

    validate_graph_structure(graph)

    output_path = Path("/tmp/graph_validated.json")
    GraphSerializer.save_to_file(graph, output_path, use_dotted=True)
    logging.info(f"Generated validated {output_path}")

    loaded_graph = GraphSerializer.load_from_file(output_path)
    logging.info(f"Successfully loaded graph with id: {loaded_graph.id}")
    script_dir = Path("/home/haxscramper/workspace/repos/hack/testing/java/elk-graph-layout-kotlin")
    script_path = script_dir / "scripts" / "run.sh"
    output_layout_path = Path("/tmp/graph_layout.json")

    cmd = local[str(script_path)].with_cwd(script_dir)
    result = cmd.run([f"--input={output_path}", f"--output={output_layout_path}"])

    logging.info(f"Layout script executed successfully, output saved to {output_layout_path}")

    laid_out_graph = GraphSerializer.load_from_file(output_layout_path)

    nodes, edges = compute_absolute_positions(laid_out_graph)
    png_output = Path("/tmp/graph_layout.png")
    render_to_png(nodes, edges, png_output)
