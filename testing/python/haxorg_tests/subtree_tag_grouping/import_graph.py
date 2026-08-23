#!/usr/bin/env python

from collections import Counter
from pathlib import Path

import click
import graphviz
import igraph
from beartype import beartype
from beartype.typing import Iterable
from google.protobuf import descriptor_pool, message_factory
import betterproto2

from gen.google.protobuf import Any as GeneratedAny
from gen.message_pool import default_message_pool
from loguru import logger

import gen.orgproto as orgproto
import gen.org.graph.proto as org_graph
import gen.hstd.ext.graph.proto as hstd_graph
from datetime import datetime

from dominate.util import raw
from dominate.tags import table, td, tr

from utils import extract_subtree_summary, paragraph_text


@beartype
def unpack_payload(
    payload: GeneratedAny | None,
    owner: str,
) -> betterproto2.Message | None:
    if payload is None or payload.type_url == "":
        return None

    message_type = default_message_pool.url_to_type.get(payload.type_url)

    if message_type is None:
        raise ValueError(
            f"Cannot unpack payload for {owner}: protobuf message "
            f"'{payload.type_url}' is not registered in the betterproto2 "
            "default message pool")

    return message_type().parse(payload.value)


@beartype
def unpack_attributes(
    attributes: Iterable[hstd_graph.IAttribute],
    owner: str,
) -> list[betterproto2.Message | None]:
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
def payload_label(payload: betterproto2.Message | None) -> str:
    if payload is None:
        return ""

    return payload.to_json(indent=2)


@beartype
def subtree_html_label(
    stable_id: str,
    subtree: orgproto.Subtree,
    now: datetime,
) -> str:
    summary = extract_subtree_summary(subtree, now)
    tags = ", ".join("/".join(path) for path in summary.tags)

    rows: list[tuple[str, str | None]] = [
        ("ID", stable_id),
        ("TODO", summary.todo),
        ("Tags", tags if tags != "" else None),
        ("Priority", summary.priority),
        ("Created", summary.created),
        ("Scheduled", summary.scheduled),
        (
            "Scheduled delta",
            str(summary.scheduled_delta_seconds)
            if summary.scheduled_delta_seconds is not None else None,
        ),
        ("Deadline", summary.deadline),
        ("Closed", summary.closed),
        ("Last clocked", summary.last_clocked),
        ("Clocked seconds", str(summary.clocked_seconds)),
        (
            "Effort minutes",
            str(summary.effort_minutes)
            if summary.effort_minutes is not None else None,
        ),
    ]

    label_table = table(
        border="0",
        cellborder="1",
        cellspacing="0",
        cellpadding="4",
    )

    with label_table:
        with tr():
            td(summary.title, colspan="2", align="left")

        for name, value in rows:
            if value is not None:
                with tr():
                    td(name, align="left")
                    td(value, align="left")

    result = "".join(("<", label_table.render(), ">"))
    logger.trace(result)
    return result


@beartype
def paragraph_html_label(
    stable_id: str,
    par: orgproto.Paragraph,
) -> str:
    import textwrap

    text = "<BR ALIGN=\"LEFT\"/>".join(
        textwrap.wrap(paragraph_text(par), width=70))

    label_table = table(
        border="0",
        cellborder="1",
        cellspacing="0",
        cellpadding="4",
    )

    with label_table:
        with tr():
            td("ID", align="left")
            td(stable_id, align="left")

        with tr():
            td(raw(text), colspan="2", align="left")

    result = "".join(("<", label_table.render(), ">"))
    logger.trace(result)
    return result


@beartype
def vertex_label(
    vertex: igraph.Vertex,
    now: datetime,
) -> str:
    payload = vertex["payload"]

    match payload:
        case org_graph.MapNodePayload(node=node) if node is not None:
            kind, node_value = betterproto2.which_one_of(node, "kind")

            if kind == "subtree":
                return subtree_html_label(
                    vertex["stable_id"],
                    node_value,
                    now,
                )

            elif kind == "paragraph":
                return paragraph_html_label(vertex["stable_id"], node_value)

            else:
                logger.warning(
                    f"Unhandled top-level payload node kind '{kind}'")

        case org_graph.MapNodePayload(node=node) if node is None:
            logger.warning("Empty map node payload")

        case _:
            pass

    return vertex["stable_id"]


@beartype
def build_graphviz(
    graph: igraph.Graph,
    now: datetime,
) -> graphviz.Digraph:
    result = graphviz.Digraph("content")
    result.attr(rankdir="LR")
    result.attr("node", shape="rect")

    for vertex in graph.vs:
        result.node(
            vertex["stable_id"],
            label=vertex_label(vertex, now),
        )

    for edge in graph.es:
        source = graph.vs[edge.source]["stable_id"]
        target = graph.vs[edge.target]["stable_id"]
        result.edge(
            source,
            target,
            # label=edge["stable_id"],
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
    visualization = build_graphviz(
        graph,
        datetime.now().astimezone(),
    )

    output_path.write_bytes(visualization.pipe(format=output_format))

    logger.info(
        f"Rendered {graph.vcount()} vertices and {graph.ecount()} edges "
        f"to '{output_path}'")


if __name__ == "__main__":
    main()
