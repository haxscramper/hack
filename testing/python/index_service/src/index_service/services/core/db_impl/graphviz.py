import html
import json
import textwrap

import graphviz

from index_service.services.core.db_impl.contracts import BaseIndexProtocol


class GraphvizMixin:

    def _format_html_label(
        self,
        data: dict,
        json_labels: bool,
        wrap_width: int = 120,
    ) -> str:

        def wrap_text(text: str) -> str:
            lines: list[str] = []

            for line in text.split("\n"):
                if len(line) <= wrap_width:
                    lines.append(line)
                    continue

                indent = len(line) - len(line.lstrip(" "))
                lines.extend(
                    textwrap.wrap(
                        line,
                        width=wrap_width,
                        subsequent_indent=" " * indent,
                        break_long_words=True,
                        break_on_hyphens=False,
                    ) or [line])

            return "\n".join(lines)

        def escape_html(text: str) -> str:
            return (html.escape(wrap_text(text)).replace(" ", "&nbsp;").replace(
                "\n", '<br align="left"/>'))

        if json_labels:
            text = json.dumps(data, indent=2, default=str)
            return (f'<<font face="monospace">'
                    f'{escape_html(text)}<br align="left"/></font>>')

        rows = []
        for key, value in data.items():
            value_text = json.dumps(value, indent=2, default=str)
            rows.append(
                f'<tr><td align="left" valign="top"><b>{html.escape(str(key))}</b></td>'
                f'<td align="left" balign="left"><font face="monospace">'
                f"{escape_html(value_text)}</font></td></tr>")

        return ('<<table border="0" cellborder="1" cellspacing="0">'
                f'{"".join(rows)}</table>>')

    def get_inbound(
        self,
        doc_id: str,
        indexer: BaseIndexProtocol,
    ) -> list[str]:
        result: list[str] = []

        for document in self.aql.execute(
                """
            FOR v, e IN 1..1 INBOUND @doc_id @@edge_collection
                RETURN v
            """,
                bind_vars={
                    "doc_id": f"{indexer.asset_name}/{doc_id}",
                    "@edge_collection": self.get_edge_name(indexer.asset_name),
                },
        ):
            result.append(document["_key"])

        return result

    def render_indexer_graphviz(
        self,
        indexer: BaseIndexProtocol,
        start_vertex_ids: set[str],
        *,
        max_nodes: int = int(1e10),
        max_depth: int = int(1e10),
        max_outgoing_edges: int = int(1e10),
        json_labels: bool = False,
    ) -> graphviz.Digraph:
        graph = graphviz.Digraph(name=self.get_graph_name(indexer.asset_name))
        edge_collection = self.get_edge_name(indexer.asset_name)
        vertex_collection = indexer.asset_name
        node_shape = "box" if json_labels else "plaintext"

        visited: set[str] = set()
        added_nodes: set[str] = set()

        def add_document_node(document: dict, *, shape: str) -> None:
            node_id = document["_id"]
            if node_id in added_nodes:
                return

            graph.node(
                node_id,
                label=self._format_html_label(document, json_labels),
                shape=shape,
                color="red",
            )
            added_nodes.add(node_id)

        def full_id(vertex_id: str) -> str:
            return (vertex_id if "/" in vertex_id else f"{vertex_collection}/{vertex_id}")

        query = """
        FOR v, e IN 1..1 OUTBOUND @start_id @@edge_collection
            LIMIT @max_outgoing_edges
            RETURN { vertex: v, edge: e }
        """

        stack = [(full_id(vertex_id), 0) for vertex_id in start_vertex_ids]

        while stack and len(added_nodes) < max_nodes:
            current_id, depth = stack.pop()
            if current_id in visited:
                continue

            visited.add(current_id)
            add_document_node(self._db.document(current_id), shape=node_shape)

            if depth >= max_depth:
                continue

            for result in self._db.aql.execute(
                    query,
                    bind_vars={
                        "start_id": current_id,
                        "@edge_collection": edge_collection,
                        "max_outgoing_edges": max_outgoing_edges,
                    },
            ):
                vertex = result["vertex"]
                edge = result["edge"]

                add_document_node(vertex, shape=node_shape)

                edge_node_id = edge["_id"]
                if edge_node_id not in added_nodes:
                    graph.node(
                        edge_node_id,
                        label=self._format_html_label(edge, json_labels),
                        shape="box",
                        color="blue",
                    )
                    added_nodes.add(edge_node_id)

                graph.edge(current_id, edge_node_id)
                graph.edge(edge_node_id, vertex["_id"])

                if vertex["_id"] not in visited:
                    stack.append((vertex["_id"], depth + 1))

        return graph
