package com.example.elk.models

import kotlinx.serialization.Serializable

@Serializable
data class InputGraph(
    val nodes: List<InputNode>,
    val edges: List<InputEdge>,
    val layoutOptions: LayoutOptions? = null
)

@Serializable
data class InputNode(
    val id: String,
    val label: String? = null,
    val width: Double? = null,
    val height: Double? = null,
    val properties: Map<String, String> = emptyMap()
)

@Serializable
data class InputEdge(
    val id: String? = null,
    val sources: List<String>,
    val targets: List<String>,
    val label: String? = null,
    val properties: Map<String, String> = emptyMap()
)

@Serializable
data class LayoutOptions(
    val direction: String? = "DOWN", // UP, DOWN, LEFT, RIGHT
    val algorithm: String? = "layered",
    val nodeSpacing: Double? = 50.0,
    val layerSpacing: Double? = 80.0,
    val edgeRouting: String? = "ORTHOGONAL", // ORTHOGONAL, POLYLINE, SPLINES
    val nodeAlignment: String? = "CENTER", // CENTER, LEFT, RIGHT
    val crossingMinimization: String? = "LAYER_SWEEP"
)
