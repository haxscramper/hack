from PyQt6.QtCore import QRectF

from graphviz_viewer.graph_viewer_v2.graph import GraphCluster, GraphNode
from graphviz_viewer.graph_viewer_v2.layout_mapper import LayoutHierarchyMapper, LayoutCluster, LayoutNode, LayoutEdge
from graphviz_viewer.graph_viewer_v2.constants import NODE_WIDTH, NODE_MINIMUM_HEIGHT, NODE_PADDING
from graphviz_viewer.graph_viewer_v2.utils import create_text_document


class DirectLayoutHierarchyMapper(LayoutHierarchyMapper):

    def map(self, root: GraphCluster) -> LayoutCluster:
        nodes_by_underlying_id: dict[str, LayoutNode] = {}

        def make_node(source: GraphNode) -> LayoutNode:
            document = create_text_document(
                source.rich_text,
                NODE_WIDTH - NODE_PADDING * 2.0,
            )
            height = max(
                NODE_MINIMUM_HEIGHT,
                document.size().height() + NODE_PADDING * 2.0,
            )

            node = LayoutNode(
                unique_id=f"layout:node:{source.unique_id}",
                underlying=source,
                related_underlying_ids=frozenset({source.unique_id}),
                properties=source.properties,
                rich_text=source.rich_text,
                rectangle=QRectF(0.0, 0.0, NODE_WIDTH, height),
            )
            nodes_by_underlying_id[source.unique_id] = node
            return node

        def convert(source: GraphCluster) -> LayoutCluster:
            result = LayoutCluster(
                unique_id=f"layout:cluster:{source.unique_id}",
                underlying=source,
                related_underlying_ids=frozenset({source.unique_id}),
                properties=source.properties,
            )

            for source_node in source.nodes:
                node = nodes_by_underlying_id.get(source_node.unique_id)

                if node is None:
                    node = make_node(source_node)

                node.parent = result
                result.nodes.append(node)

            for source_cluster in source.clusters:
                child = convert(source_cluster)
                child.parent = result
                result.clusters.append(child)

            for source_edge in source.edges:
                source_node = nodes_by_underlying_id.get(
                    source_edge.source.unique_id)
                target_node = nodes_by_underlying_id.get(
                    source_edge.target.unique_id)

                if source_node is None:
                    source_node = make_node(source_edge.source)
                    source_node.parent = result
                    result.nodes.append(source_node)

                if target_node is None:
                    target_node = make_node(source_edge.target)
                    target_node.parent = result
                    result.nodes.append(target_node)

                edge = LayoutEdge(
                    unique_id=f"layout:edge:{source_edge.unique_id}",
                    underlying=source_edge,
                    related_underlying_ids=frozenset({source_edge.unique_id}),
                    properties=source_edge.properties,
                    source=source_node,
                    target=target_node,
                    rich_text=source_edge.rich_text,
                )
                edge.parent = result
                result.edges.append(edge)

            return result

        return convert(root)
