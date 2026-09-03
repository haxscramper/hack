#!/usr/bin/env python

from __future__ import annotations

import html
from abc import ABC, abstractmethod
from functools import cached_property
from html.parser import HTMLParser
from pathlib import Path
from typing import Mapping, Sequence

import pydot


def unquote_dot_value(value: str | None) -> str:
    if value is None:
        return ""

    result = str(value).strip()

    if len(result) >= 2 and result[0] == '"' and result[-1] == '"':
        result = result[1:-1]
        result = result.replace(r"\"", '"')
        result = result.replace(r"\\", "\\")

    return result


def display_dot_value(value: str | None) -> str:
    return (unquote_dot_value(value).replace(r"\l", "\n").replace(
        r"\r", "\n").replace(r"\n", "\n"))


class GraphvizHtmlConverter(HTMLParser):
    allowed_inline_tags = {
        "b",
        "i",
        "u",
        "o",
        "s",
        "sub",
        "sup",
    }

    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.output: list[str] = []
        self.open_tags: list[str | None] = []
        self.table_stack: list[dict[str, str]] = []

    @staticmethod
    def attributes_dict(
        attributes: list[tuple[str, str | None]], ) -> dict[str, str]:
        return {name.lower(): value or "" for name, value in attributes}

    def handle_starttag(
        self,
        tag: str,
        attributes: list[tuple[str, str | None]],
    ) -> None:
        tag = tag.lower()
        attrs = self.attributes_dict(attributes)

        if tag == "table":
            border = attrs.get("border", "0")
            cellborder = attrs.get("cellborder", "0")
            cellspacing = attrs.get("cellspacing", "0")
            cellpadding = attrs.get("cellpadding", "3")

            styles = [
                "border-collapse: separate",
                f"border-spacing: {html.escape(cellspacing)}px",
            ]

            if border not in {"", "0"}:
                styles.append(f"border: {html.escape(border)}px solid #606770")

            if bgcolor := attrs.get("bgcolor"):
                styles.append(
                    f"background-color: {html.escape(bgcolor, quote=True)}")

            self.output.append(
                '<table width="100%" '
                f'cellspacing="{html.escape(cellspacing, quote=True)}" '
                f'cellpadding="{html.escape(cellpadding, quote=True)}" '
                f'style="{html.escape("; ".join(styles), quote=True)}">')
            self.table_stack.append({"cellborder": cellborder})
            self.open_tags.append("table")
            return

        if tag == "tr":
            self.output.append("<tr>")
            self.open_tags.append("tr")
            return

        if tag == "td":
            styles: list[str] = []
            cellborder = (self.table_stack[-1].get("cellborder", "0")
                          if self.table_stack else "0")

            if cellborder not in {"", "0"}:
                styles.append(
                    f"border: {html.escape(cellborder)}px solid #606770")

            align = attrs.get("align", "").lower()
            if align in {"left", "right", "center", "justify"}:
                styles.append(f"text-align: {align}")

            valign = attrs.get("valign", "").lower()
            if valign in {"top", "middle", "bottom"}:
                styles.append(f"vertical-align: {valign}")

            if bgcolor := attrs.get("bgcolor"):
                styles.append(
                    f"background-color: {html.escape(bgcolor, quote=True)}")

            output_attributes: list[str] = []

            for name in ("colspan", "rowspan"):
                if value := attrs.get(name):
                    output_attributes.append(
                        f'{name}="{html.escape(value, quote=True)}"')

            if styles:
                output_attributes.append(
                    f'style="{html.escape("; ".join(styles), quote=True)}"')

            suffix = (" " +
                      " ".join(output_attributes) if output_attributes else "")
            self.output.append(f"<td{suffix}>")
            self.open_tags.append("td")
            return

        if tag == "br":
            self.output.append("<br/>")
            self.open_tags.append(None)
            return

        if tag == "font":
            styles: list[str] = []

            if color := attrs.get("color"):
                styles.append(f"color: {html.escape(color, quote=True)}")

            if face := attrs.get("face"):
                styles.append(
                    f"font-family: '{html.escape(face, quote=True)}'")

            if point_size := attrs.get("point-size"):
                styles.append(
                    f"font-size: {html.escape(point_size, quote=True)}pt")

            style = html.escape("; ".join(styles), quote=True)
            self.output.append(f'<span style="{style}">')
            self.open_tags.append("span")
            return

        if tag in self.allowed_inline_tags:
            output_tag = "s" if tag == "o" else tag
            self.output.append(f"<{output_tag}>")
            self.open_tags.append(output_tag)
            return

        self.open_tags.append(None)

    def handle_startendtag(
        self,
        tag: str,
        attributes: list[tuple[str, str | None]],
    ) -> None:
        if tag.lower() == "br":
            self.output.append("<br/>")
            return

        self.handle_starttag(tag, attributes)
        self.handle_endtag(tag)

    def handle_endtag(self, tag: str) -> None:
        if not self.open_tags:
            return

        output_tag = self.open_tags.pop()

        if tag.lower() == "table" and self.table_stack:
            self.table_stack.pop()

        if output_tag is not None:
            self.output.append(f"</{output_tag}>")

    def handle_data(self, data: str) -> None:
        self.output.append(html.escape(data))

    def result(self) -> str:
        while self.open_tags:
            output_tag = self.open_tags.pop()

            if output_tag is not None:
                self.output.append(f"</{output_tag}>")

        return "".join(self.output)


def graphviz_label_to_qt_html(label: str | None) -> str:
    raw_label = str(label or "").strip()

    if raw_label.startswith("<<") and raw_label.endswith(">>"):
        converter = GraphvizHtmlConverter()
        converter.feed(raw_label[1:-1])
        converter.close()
        body = converter.result()
    else:
        body = html.escape(display_dot_value(raw_label))
        body = body.replace("\n", "<br/>")

    return ("<html><head><style>"
            "body { color: #202124; font-family: sans-serif; }"
            "table { width: 100%; }"
            "td { padding: 3px; }"
            "</style></head>"
            f"<body>{body}</body></html>")


class GraphElement(ABC):

    @property
    @abstractmethod
    def unique_id(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def properties(self) -> Mapping[str, str]:
        raise NotImplementedError


class GraphNode(GraphElement, ABC):

    @property
    @abstractmethod
    def rich_text(self) -> str:
        raise NotImplementedError


class GraphEdge(GraphElement, ABC):

    @property
    @abstractmethod
    def source(self) -> GraphNode:
        raise NotImplementedError

    @property
    @abstractmethod
    def target(self) -> GraphNode:
        raise NotImplementedError

    @property
    @abstractmethod
    def rich_text(self) -> str:
        raise NotImplementedError


class GraphCluster(GraphElement, ABC):

    @property
    @abstractmethod
    def nodes(self) -> Sequence[GraphNode]:
        raise NotImplementedError

    @property
    @abstractmethod
    def edges(self) -> Sequence[GraphEdge]:
        raise NotImplementedError

    @property
    @abstractmethod
    def clusters(self) -> Sequence[GraphCluster]:
        raise NotImplementedError


class GraphProvider(ABC):

    @abstractmethod
    def read(self, path: Path) -> GraphCluster:
        raise NotImplementedError


class UniqueIdAllocator:

    def __init__(self) -> None:
        self.used: set[str] = set()

    def allocate(self, requested: str, prefix: str) -> str:
        base = requested.strip() or prefix
        candidate = base
        suffix = 2

        while candidate in self.used:
            candidate = f"{base}#{suffix}"
            suffix += 1

        self.used.add(candidate)
        return candidate


class GraphvizNode(GraphNode):

    def __init__(
        self,
        unique_id: str,
        name: str,
        attributes: Mapping[str, str],
    ) -> None:
        self._unique_id = unique_id
        self.name = name
        self._properties = dict(attributes)

    @property
    def unique_id(self) -> str:
        return self._unique_id

    @property
    def properties(self) -> Mapping[str, str]:
        return self._properties

    @cached_property
    def rich_text(self) -> str:
        label = self._properties.get("label", self.name)
        return graphviz_label_to_qt_html(label)


class GraphvizEdge(GraphEdge):

    def __init__(
        self,
        unique_id: str,
        source: GraphNode,
        target: GraphNode,
        attributes: Mapping[str, str],
    ) -> None:
        self._unique_id = unique_id
        self._source = source
        self._target = target
        self._properties = dict(attributes)

    @property
    def unique_id(self) -> str:
        return self._unique_id

    @property
    def source(self) -> GraphNode:
        return self._source

    @property
    def target(self) -> GraphNode:
        return self._target

    @property
    def properties(self) -> Mapping[str, str]:
        return self._properties

    @cached_property
    def rich_text(self) -> str:
        label = self._properties.get("label", "")

        if not display_dot_value(label).strip():
            return ""

        return graphviz_label_to_qt_html(label)


class GraphvizCluster(GraphCluster):

    def __init__(
        self,
        unique_id: str,
        name: str,
        properties: Mapping[str, str],
    ) -> None:
        self._unique_id = unique_id
        self.name = name
        self._properties = dict(properties)
        self._nodes: list[GraphNode] = []
        self._edges: list[GraphEdge] = []
        self._clusters: list[GraphCluster] = []

    @property
    def unique_id(self) -> str:
        return self._unique_id

    @property
    def properties(self) -> Mapping[str, str]:
        return self._properties

    @property
    def nodes(self) -> Sequence[GraphNode]:
        return self._nodes

    @property
    def edges(self) -> Sequence[GraphEdge]:
        return self._edges

    @property
    def clusters(self) -> Sequence[GraphCluster]:
        return self._clusters


class GraphvizGraphProvider(GraphProvider):

    def read(self, path: Path) -> GraphCluster:
        graphs = pydot.graph_from_dot_data(path.read_text(encoding="utf-8"))

        if not graphs:
            raise ValueError("The input did not contain a Graphviz graph")

        graph = graphs[0]
        allocator = UniqueIdAllocator()
        nodes_by_name: dict[str, GraphvizNode] = {}

        def ensure_node(name: str) -> GraphvizNode:
            normalized = unquote_dot_value(name).strip()

            if normalized not in nodes_by_name:
                nodes_by_name[normalized] = GraphvizNode(
                    allocator.allocate(normalized, "node"),
                    normalized,
                    {},
                )

            return nodes_by_name[normalized]

        def convert_cluster(
            source: pydot.Graph,
            is_root: bool,
        ) -> GraphvizCluster:
            source_name = unquote_dot_value(source.get_name())
            requested_id = source_name or ("root" if is_root else "cluster")

            result = GraphvizCluster(
                allocator.allocate(requested_id, "cluster"),
                source_name or "Graph",
                {
                    key: str(value)
                    for key, value in source.get_attributes().items()
                },
            )

            for raw_node in source.get_nodes():
                name = unquote_dot_value(raw_node.get_name()).strip()

                if name in {"", "graph", "node", "edge", r"\n"}:
                    continue

                attributes = {
                    key: str(value)
                    for key, value in raw_node.get_attributes().items()
                }

                if name in nodes_by_name:
                    node = nodes_by_name[name]
                    node._properties.update(attributes)
                else:
                    node = GraphvizNode(
                        allocator.allocate(name, "node"),
                        name,
                        attributes,
                    )
                    nodes_by_name[name] = node

                if node not in result._nodes:
                    result._nodes.append(node)

            for subgraph in source.get_subgraphs():
                result._clusters.append(convert_cluster(subgraph, False))

            for raw_edge in source.get_edges():
                source_node = ensure_node(raw_edge.get_source())
                target_node = ensure_node(raw_edge.get_destination())
                attributes = {
                    key: str(value)
                    for key, value in raw_edge.get_attributes().items()
                }
                requested_edge_id = unquote_dot_value(attributes.get("id"))

                result._edges.append(
                    GraphvizEdge(
                        allocator.allocate(requested_edge_id, "edge"),
                        source_node,
                        target_node,
                        attributes,
                    ))

            return result

        root = convert_cluster(graph, True)

        assigned_nodes = {
            node
            for cluster in self._walk_clusters(root)
            for node in cluster.nodes
        }

        for node in nodes_by_name.values():
            if node not in assigned_nodes:
                root._nodes.append(node)

        return root

    @staticmethod
    def _walk_clusters(root: GraphCluster, ) -> Sequence[GraphCluster]:
        result: list[GraphCluster] = [root]

        for child in root.clusters:
            result.extend(GraphvizGraphProvider._walk_clusters(child))

        return result
