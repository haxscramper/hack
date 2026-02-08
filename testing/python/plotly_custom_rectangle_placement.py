#!/usr/bin/env python

import plotly.graph_objects as go
from dataclasses import dataclass, field
from typing import List, Optional


@dataclass
class TreeNode:
    """Represents a node in the tree with a range."""
    name: str
    start: float
    end: float
    children: List['TreeNode'] = field(default_factory=list)

    def __post_init__(self):
        if self.children is None:
            self.children = []


def calculate_layout(node: TreeNode,
                     depth: int = 0,
                     y_offset: float = 0,
                     layout_info: dict = None) -> dict:
    """
    Calculate the layout positions for all nodes.
    Returns a dict mapping node names to their layout info.
    """
    if layout_info is None:
        layout_info = {'nodes': [], 'max_depth': 0}

    # Calculate height for this node (based on subtree)
    height = calculate_subtree_height(node)

    # Store this node's layout info
    layout_info['nodes'].append({
        'name': node.name,
        'start': node.start,
        'end': node.end,
        'depth': depth,
        'y_bottom': y_offset,
        'y_top': y_offset + height,
        'height': height
    })

    layout_info['max_depth'] = max(layout_info['max_depth'], depth)

    # Layout children
    child_y_offset = y_offset
    for child in node.children:
        calculate_layout(child, depth + 1, child_y_offset, layout_info)
        child_y_offset += calculate_subtree_height(child)

    return layout_info


def calculate_subtree_height(node: TreeNode) -> float:
    """Calculate the total height needed for a node and its subtree."""
    if not node.children:
        return 1.0  # Leaf nodes have height 1
    return sum(calculate_subtree_height(child) for child in node.children)


def create_nested_range_visualization(
        root: TreeNode,
        title: str = "Nested Range Visualization",
        color_by_depth: bool = True) -> go.Figure:
    """
    Create an interactive Plotly visualization of nested ranges.
    
    Args:
        root: The root node of the tree
        title: Chart title
        color_by_depth: If True, color rectangles by depth level
    
    Returns:
        A Plotly Figure object
    """
    # Calculate layout
    layout_info = calculate_layout(root)
    nodes = layout_info['nodes']
    max_depth = layout_info['max_depth']

    # Color palette for different depths
    colors = [
        'rgba(31, 119, 180, 0.7)',  # blue
        'rgba(255, 127, 14, 0.7)',  # orange
        'rgba(44, 160, 44, 0.7)',  # green
        'rgba(214, 39, 40, 0.7)',  # red
        'rgba(148, 103, 189, 0.7)',  # purple
        'rgba(140, 86, 75, 0.7)',  # brown
        'rgba(227, 119, 194, 0.7)',  # pink
        'rgba(127, 127, 127, 0.7)',  # gray
    ]

    border_colors = [
        'rgba(31, 119, 180, 1)',
        'rgba(255, 127, 14, 1)',
        'rgba(44, 160, 44, 1)',
        'rgba(214, 39, 40, 1)',
        'rgba(148, 103, 189, 1)',
        'rgba(140, 86, 75, 1)',
        'rgba(227, 119, 194, 1)',
        'rgba(127, 127, 127, 1)',
    ]

    fig = go.Figure()

    # Sort nodes by depth (draw deeper nodes on top)
    nodes_sorted = sorted(nodes, key=lambda x: x['depth'])

    shapes = []
    annotations = []

    # Create invisible scatter trace for hover info
    hover_x = []
    hover_y = []
    hover_text = []
    hover_colors = []

    for node_info in nodes_sorted:
        depth = node_info['depth']
        color_idx = depth % len(colors)

        # Add small padding based on depth for visual nesting effect
        padding = depth * 0.02

        # Create rectangle shape
        shapes.append(
            dict(type="rect",
                 x0=node_info['start'],
                 x1=node_info['end'],
                 y0=node_info['y_bottom'] + padding,
                 y1=node_info['y_top'] - padding,
                 fillcolor=colors[color_idx]
                 if color_by_depth else 'rgba(100, 150, 200, 0.5)',
                 line=dict(color=border_colors[color_idx], width=2),
                 layer="above"))

        # Add hover point at center of rectangle
        center_x = (node_info['start'] + node_info['end']) / 2
        center_y = (node_info['y_bottom'] + node_info['y_top']) / 2
        hover_x.append(center_x)
        hover_y.append(center_y)
        hover_text.append(
            f"<b>{node_info['name']}</b><br>"
            f"Range: [{node_info['start']}, {node_info['end']}]<br>"
            f"Duration: {node_info['end'] - node_info['start']}<br>"
            f"Depth: {depth}")
        hover_colors.append(colors[color_idx])

        # Add label annotation
        annotations.append(
            dict(x=center_x,
                 y=center_y,
                 text=node_info['name'],
                 showarrow=False,
                 font=dict(size=10, color='black'),
                 xanchor='center',
                 yanchor='middle'))

    # Add invisible scatter for hover interactivity
    fig.add_trace(
        go.Scatter(x=hover_x,
                   y=hover_y,
                   mode='markers',
                   marker=dict(size=20, opacity=0),
                   hoverinfo='text',
                   hovertext=hover_text,
                   showlegend=False))

    # Add legend traces for depth levels
    for d in range(max_depth + 1):
        fig.add_trace(
            go.Scatter(x=[None],
                       y=[None],
                       mode='markers',
                       marker=dict(size=15,
                                   color=colors[d % len(colors)],
                                   symbol='square'),
                       name=f'Depth {d}',
                       showlegend=True))

    # Update layout
    all_starts = [n['start'] for n in nodes]
    all_ends = [n['end'] for n in nodes]
    x_min, x_max = min(all_starts), max(all_ends)
    x_padding = (x_max - x_min) * 0.05

    max_y = max(n['y_top'] for n in nodes)

    fig.update_layout(title=dict(text=title, font=dict(size=16)),
                      shapes=shapes,
                      annotations=annotations,
                      xaxis=dict(title="Range",
                                 range=[x_min - x_padding, x_max + x_padding],
                                 showgrid=True,
                                 gridcolor='lightgray'),
                      yaxis=dict(title="",
                                 range=[-0.5, max_y + 0.5],
                                 showticklabels=False,
                                 showgrid=False),
                      plot_bgcolor='white',
                      hoverlabel=dict(bgcolor="white", font_size=12),
                      legend=dict(title="Depth Level",
                                  yanchor="top",
                                  y=0.99,
                                  xanchor="left",
                                  x=1.02),
                      margin=dict(r=150))

    return fig


# Alternative: Icicle-style visualization (top-down nesting)
def create_icicle_range_visualization(
        root: TreeNode,
        title: str = "Icicle Range Visualization") -> go.Figure:
    """
    Create an icicle-style visualization where depth is shown vertically
    and ranges are shown horizontally.
    """

    def collect_nodes(node: TreeNode, depth: int = 0, nodes_list: list = None):
        if nodes_list is None:
            nodes_list = []
        nodes_list.append({
            'name': node.name,
            'start': node.start,
            'end': node.end,
            'depth': depth
        })
        for child in node.children:
            collect_nodes(child, depth + 1, nodes_list)
        return nodes_list

    nodes = collect_nodes(root)
    max_depth = max(n['depth'] for n in nodes)

    colors = [
        'rgba(31, 119, 180, 0.8)',
        'rgba(255, 127, 14, 0.8)',
        'rgba(44, 160, 44, 0.8)',
        'rgba(214, 39, 40, 0.8)',
        'rgba(148, 103, 189, 0.8)',
    ]

    fig = go.Figure()
    shapes = []
    annotations = []

    row_height = 1.0

    for node_info in sorted(nodes, key=lambda x: x['depth']):
        depth = node_info['depth']
        y_top = (max_depth - depth + 1) * row_height
        y_bottom = y_top - row_height * 0.9

        shapes.append(
            dict(
                type="rect",
                x0=node_info['start'],
                x1=node_info['end'],
                y0=y_bottom,
                y1=y_top,
                fillcolor=colors[depth % len(colors)],
                line=dict(color='white', width=2),
            ))

        center_x = (node_info['start'] + node_info['end']) / 2
        center_y = (y_top + y_bottom) / 2

        annotations.append(
            dict(
                x=center_x,
                y=center_y,
                text=node_info['name'],
                showarrow=False,
                font=dict(size=10, color='white', family='Arial Black'),
            ))

    all_starts = [n['start'] for n in nodes]
    all_ends = [n['end'] for n in nodes]
    x_min, x_max = min(all_starts), max(all_ends)

    fig.update_layout(
        title=title,
        shapes=shapes,
        annotations=annotations,
        xaxis=dict(title="Range", range=[x_min - 1, x_max + 1]),
        yaxis=dict(title="Depth",
                   range=[0, (max_depth + 2) * row_height],
                   tickvals=[(max_depth - d + 0.5) * row_height
                             for d in range(max_depth + 1)],
                   ticktext=[f"Level {d}" for d in range(max_depth + 1)]),
        plot_bgcolor='white',
    )

    return fig


# Example usage and demo
if __name__ == "__main__":
    # Create sample tree data
    root = TreeNode(name="Root",
                    start=0,
                    end=100,
                    children=[
                        TreeNode(name="A",
                                 start=5,
                                 end=45,
                                 children=[
                                     TreeNode(name="A1", start=10, end=25),
                                     TreeNode(name="A2", start=28, end=42),
                                 ]),
                        TreeNode(name="B",
                                 start=50,
                                 end=95,
                                 children=[
                                     TreeNode(name="B1",
                                              start=52,
                                              end=70,
                                              children=[
                                                  TreeNode(name="B1a",
                                                           start=55,
                                                           end=62),
                                                  TreeNode(name="B1b",
                                                           start=64,
                                                           end=68),
                                              ]),
                                     TreeNode(name="B2", start=75, end=92),
                                 ]),
                    ])

    # Create and show the nested visualization
    fig1 = create_nested_range_visualization(
        root, title="Nested Range Visualization (Treemap-style)")
    fig1.show()

    # Create and show the icicle visualization
    fig2 = create_icicle_range_visualization(
        root, title="Icicle Range Visualization")
    fig2.show()
