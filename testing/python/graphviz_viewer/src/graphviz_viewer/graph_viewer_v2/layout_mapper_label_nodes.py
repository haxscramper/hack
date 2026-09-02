from PyQt6.QtCore import QRectF

from graphviz_viewer.graph_viewer_v2.constants import NODE_PADDING, NODE_MINIMUM_HEIGHT, NODE_WIDTH
from graphviz_viewer.graph_viewer_v2.graph import GraphCluster
from graphviz_viewer.graph_viewer_v2.layout_mapper import LayoutCluster, LayoutEdge, LayoutNode
from graphviz_viewer.graph_viewer_v2.layout_mapper import LayoutHierarchyMapper
from graphviz_viewer.graph_viewer_v2.layout_mapper_direct import DirectLayoutHierarchyMapper
from graphviz_viewer.graph_viewer_v2.utils import create_text_document


class EdgeLabelLayoutHierarchyMapper(LayoutHierarchyMapper):

    def __init__(self) -> None:
        self.direct_mapper = DirectLayoutHierarchyMapper()

    def map(self, root: GraphCluster) -> LayoutCluster:
        layout_root = self.direct_mapper.map(root)

        def split_labels(cluster: LayoutCluster) -> None:
            replacement_edges: list[LayoutEdge] = []

            for edge in cluster.edges:
                if not edge.rich_text.strip():
                    replacement_edges.append(edge)
                    continue

                document = create_text_document(
                    edge.rich_text,
                    NODE_WIDTH - NODE_PADDING * 2.0,
                )
                height = max(
                    NODE_MINIMUM_HEIGHT,
                    document.size().height() + NODE_PADDING * 2.0,
                )
                related = frozenset({edge.underlying.unique_id})

                label_node = LayoutNode(
                    unique_id=f"{edge.unique_id}:label",
                    underlying=edge.underlying,
                    related_underlying_ids=related,
                    properties=edge.properties,
                    rich_text=edge.rich_text,
                    rectangle=QRectF(
                        0.0,
                        0.0,
                        NODE_WIDTH,
                        height,
                    ),
                    parent=cluster,
                )
                cluster.nodes.append(label_node)

                tail = LayoutEdge(
                    unique_id=f"{edge.unique_id}:tail",
                    underlying=edge.underlying,
                    related_underlying_ids=related,
                    properties=edge.properties,
                    source=edge.source,
                    target=label_node,
                    parent=cluster,
                )
                head = LayoutEdge(
                    unique_id=f"{edge.unique_id}:head",
                    underlying=edge.underlying,
                    related_underlying_ids=related,
                    properties=edge.properties,
                    source=label_node,
                    target=edge.target,
                    parent=cluster,
                )
                replacement_edges.extend((tail, head))

            cluster.edges = replacement_edges

            for child in cluster.clusters:
                split_labels(child)

        split_labels(layout_root)
        return layout_root
