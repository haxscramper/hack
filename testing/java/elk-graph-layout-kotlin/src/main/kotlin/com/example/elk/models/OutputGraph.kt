package com.example.elk.models

import kotlinx.serialization.Serializable

@Serializable
data class OutputGraph(
    val nodes: List<OutputNode>,
    val edges: List<OutputEdge>,
    val bounds: GraphBounds,
    val layoutInfo: LayoutInfo
)

@Serializable
data class OutputNode(
    val id: String,
    val label: String? = null,
    val position: Position,
    val size: Size,
    val properties: Map<String, String> = emptyMap()
)

@Serializable
data class OutputEdge(
    val id: String? = null,
    val sources: List<String>,
    val targets: List<String>,
    val label: String? = null,
    val points: List<Position> = emptyList(),
    val properties: Map<String, String> = emptyMap()
)

@Serializable
data class Position(
    val x: Double,
    val y: Double
)

@Serializable
data class Size(
    val width: Double,
    val height: Double
)

@Serializable
data class GraphBounds(
    val x: Double,
    val y: Double,
    val width: Double,
    val height: Double
)

@Serializable
data class LayoutInfo(
    val algorithm: String,
    val direction: String,
    val executionTime: Long, // milliseconds
    val nodeCount: Int,
    val edgeCount: Int
)
