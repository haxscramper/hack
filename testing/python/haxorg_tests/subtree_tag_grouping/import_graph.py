#!/usr/bin/env python

from collections import Counter
from pathlib import Path

import click
import graphviz
import igraph
from beartype import beartype
from beartype.typing import Iterable
from google.protobuf import descriptor_pool, message_factory
from gen.google.protobuf import Any as GeneratedAny
from google.protobuf.any_pb2 import Any as ProtobufAny
from google.protobuf.message import Message
from google.protobuf.text_format import MessageToString
from loguru import logger

import gen.orgproto as orgproto
import gen.org.graph.proto as org_graph
import gen.hstd.ext.graph.proto as hstd_graph


@beartype
def unpack_payload(
    payload: GeneratedAny | None,
    owner: str,
) -> Message | None:
    if payload is None or payload.type_url == "":
        return None

    protobuf_payload = ProtobufAny(
        type_url=payload.type_url,
        value=payload.value,
    )
    message_name = protobuf_payload.TypeName()

    try:
        descriptor = descriptor_pool.Default().FindMessageTypeByName(
            message_name)
    except KeyError as exception:
        raise ValueError(
            f"Cannot unpack payload for {owner}: protobuf message "
            f"'{message_name}' from '{payload.type_url}' is not registered"
        ) from exception

    message_type = message_factory.GetMessageClass(descriptor)
    message = message_type()

    if not protobuf_payload.Unpack(message):
        raise ValueError(
            f"Cannot unpack payload for {owner}: '{payload.type_url}' could "
            f"not be unpacked as '{message_name}'")

    return message


@beartype
def unpack_attributes(
    attributes: Iterable[hstd_graph.IAttribute],
    owner: str,
) -> list[Message | None]:
    return [
        unpack_payload(attribute.payload,
                       f"{owner} attribute '{attribute.type}'")
        for attribute in attributes
    ]


@beartype
def collect_edges(
        graph_proto: hstd_graph.IGraphProto) -> list[hstd_graph.IEdge]:
    edges: list[hstd_graph.IEdge] = []

    for collection in graph_proto.collections:
        edges.extend(collection.edges)

    for hierarchy in graph_proto.hierarchies:
        edges.extend(hierarchy.edges)

    return edges


@beartype
def build_igraph(graph_proto: hstd_graph.IGraphProto) -> igraph.Graph:
    stable_ids = [vertex.stable_id for vertex in graph_proto.vertices]
    duplicate_ids = sorted(stable_id
                           for stable_id, count in Counter(stable_ids).items()
                           if count != 1)

    if duplicate_ids:
        raise ValueError(
            "Cannot construct graph because these vertex stable IDs are "
            f"duplicated: {', '.join(duplicate_ids)}")

    graph = igraph.Graph(directed=True)
    vertex_indices: dict[str, int] = {}

    for source_vertex in graph_proto.vertices:
        vertex_index = graph.vcount()
        vertex_indices[source_vertex.stable_id] = vertex_index
        graph.add_vertex(
            name=source_vertex.stable_id,
            stable_id=source_vertex.stable_id,
            type=source_vertex.type,
            attributes=unpack_attributes(
                source_vertex.attributes,
                f"vertex '{source_vertex.stable_id}'",
            ),
            payload=unpack_payload(
                source_vertex.payload,
                f"vertex '{source_vertex.stable_id}'",
            ),
            protobuf=source_vertex,
        )

    for source_edge in collect_edges(graph_proto):
        missing_ids = [
            vertex_id for vertex_id in (
                source_edge.source_vertex_id,
                source_edge.target_vertex_id,
            ) if vertex_id not in vertex_indices
        ]

        if missing_ids:
            raise ValueError(
                f"Edge '{source_edge.stable_id}' references missing vertices: "
                f"{', '.join(missing_ids)}")

        graph.add_edge(
            vertex_indices[source_edge.source_vertex_id],
            vertex_indices[source_edge.target_vertex_id],
            stable_id=source_edge.stable_id,
            type=source_edge.type,
            attributes=unpack_attributes(
                source_edge.attributes,
                f"edge '{source_edge.stable_id}'",
            ),
            payload=unpack_payload(
                source_edge.payload,
                f"edge '{source_edge.stable_id}'",
            ),
            protobuf=source_edge,
        )

    return graph


@beartype
def payload_label(payload: Message | None) -> str:
    if payload is None:
        return ""

    return MessageToString(payload).rstrip()


@beartype
def build_graphviz(graph: igraph.Graph) -> graphviz.Digraph:
    result = graphviz.Digraph("content")
    result.attr(rankdir="LR")
    result.attr("node", shape="rect")

    for vertex in graph.vs:
        label_parts = [vertex["stable_id"]]
        payload_text = payload_label(vertex["payload"])

        if payload_text != "":
            label_parts.append(payload_text)

        result.node(
            vertex["stable_id"],
            label="\n".join(label_parts),
        )

    for edge in graph.es:
        source = graph.vs[edge.source]["stable_id"]
        target = graph.vs[edge.target]["stable_id"]
        result.edge(
            source,
            target,
            label=edge["stable_id"],
        )

    return result


@beartype
def load_graph(path: Path) -> hstd_graph.IGraphProto:
    return hstd_graph.IGraphProto().parse(path.read_bytes())


@click.command()
@click.argument(
    "input_path",
    type=click.Path(
        exists=True,
        file_okay=True,
        dir_okay=False,
        path_type=Path,
    ),
)
@click.argument(
    "output_path",
    type=click.Path(
        file_okay=True,
        dir_okay=False,
        path_type=Path,
    ),
)
@beartype
def main(input_path: Path, output_path: Path) -> None:
    output_format = output_path.suffix.removeprefix(".")

    if output_format == "":
        raise click.BadParameter(
            f"Output path '{output_path}' has no Graphviz format extension",
            param_hint="output_path",
        )

    graph_proto = load_graph(input_path)
    graph = build_igraph(graph_proto)
    visualization = build_graphviz(graph)
    output_path.write_bytes(visualization.pipe(format=output_format))

    logger.info(
        f"Rendered {graph.vcount()} vertices and {graph.ecount()} edges "
        f"to '{output_path}'")


if __name__ == "__main__":
    main()
