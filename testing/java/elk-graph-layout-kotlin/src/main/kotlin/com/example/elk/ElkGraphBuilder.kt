package com.example.elk

import com.example.elk.models.*
import org.eclipse.elk.alg.layered.options.LayeredOptions
import org.eclipse.elk.core.options.CoreOptions
import org.eclipse.elk.core.options.Direction
import org.eclipse.elk.core.options.EdgeRouting
import org.eclipse.elk.alg.layered.options.CrossingMinimizationStrategy
import org.eclipse.elk.graph.ElkEdge
import org.eclipse.elk.graph.ElkNode
import org.eclipse.elk.graph.util.ElkGraphUtil

class ElkGraphBuilder {
    
    fun buildElkGraph(inputGraph: InputGraph): ElkNode {
        val graph = ElkGraphUtil.createGraph()
        
        // Configure layout options
        configureLayoutOptions(graph, inputGraph.layoutOptions)
        
        // Create nodes
        val nodeMap = mutableMapOf<String, ElkNode>()
        inputGraph.nodes.forEach { inputNode ->
            val elkNode = createElkNode(graph, inputNode)
            nodeMap[inputNode.id] = elkNode
        }

        // Create edges
        inputGraph.edges.forEach { inputEdge ->
            val sourceNodes = inputEdge.sources.mapNotNull { nodeMap[it] }
            val targetNodes = inputEdge.targets.mapNotNull { nodeMap[it] }

            if (sourceNodes.isNotEmpty() && targetNodes.isNotEmpty()) {
                createElkEdge(graph, sourceNodes, targetNodes, inputEdge)
            } else {
                println("Warning: Edge ${inputEdge.id} references non-existent nodes")
            }
        }

        return graph
    }
    
    private fun configureLayoutOptions(graph: ElkNode, options: LayoutOptions?) {
        val layoutOptions = options ?: LayoutOptions()
        
        // Set algorithm
        graph.setProperty(CoreOptions.ALGORITHM, LayeredOptions.ALGORITHM_ID)
        
        // Set direction
        val direction = when (layoutOptions.direction?.uppercase()) {
            "UP" -> Direction.UP
            "DOWN" -> Direction.DOWN
            "LEFT" -> Direction.LEFT
            "RIGHT" -> Direction.RIGHT
            else -> Direction.DOWN
        }
        graph.setProperty(CoreOptions.DIRECTION, direction)
        
        // Set spacing
        layoutOptions.nodeSpacing?.let {
            graph.setProperty(LayeredOptions.SPACING_NODE_NODE, it)
        }
        
        layoutOptions.layerSpacing?.let {
            graph.setProperty(LayeredOptions.SPACING_NODE_NODE_BETWEEN_LAYERS, it)
        }
        
        // Set edge routing - Using correct import
        val edgeRouting = when (layoutOptions.edgeRouting?.uppercase()) {
            "ORTHOGONAL" -> EdgeRouting.ORTHOGONAL
            "POLYLINE" -> EdgeRouting.POLYLINE
            "SPLINES" -> EdgeRouting.SPLINES
            else -> EdgeRouting.ORTHOGONAL
        }
        graph.setProperty(CoreOptions.EDGE_ROUTING, edgeRouting)
        
        // Set crossing minimization - Using correct import
        val crossingMin = when (layoutOptions.crossingMinimization?.uppercase()) {
            "LAYER_SWEEP" -> CrossingMinimizationStrategy.LAYER_SWEEP
            "INTERACTIVE" -> CrossingMinimizationStrategy.INTERACTIVE
            else -> CrossingMinimizationStrategy.LAYER_SWEEP
        }
        graph.setProperty(LayeredOptions.CROSSING_MINIMIZATION_STRATEGY, crossingMin)
    }
    
    private fun createElkNode(parent: ElkNode, inputNode: InputNode): ElkNode {
        val elkNode = ElkGraphUtil.createNode(parent)
        
        // Set dimensions
        elkNode.width = inputNode.width ?: 120.0
        elkNode.height = inputNode.height ?: 60.0
        
        // Add label if provided
        inputNode.label?.let { labelText ->
            val label = ElkGraphUtil.createLabel(elkNode)
            label.text = labelText
        }
        
        return elkNode
    }

    private fun createElkEdge(
        graph: ElkNode,
        sources: List<ElkNode>,
        targets: List<ElkNode>,
        inputEdge: InputEdge
    ): ElkEdge {
        // Create edge with the parent graph
        val elkEdge = ElkGraphUtil.createEdge(null)

        // Add sources and targets
        sources.forEach { elkEdge.sources.add(it) }
        targets.forEach { elkEdge.targets.add(it) }

        // Note: graph.containedEdges.add(elkEdge) is not needed
        // as createEdge(graph) already adds it to the graph

        // Add label if provided
        inputEdge.label?.let { labelText ->
            val label = ElkGraphUtil.createLabel(elkEdge)
            label.text = labelText
        }

        return elkEdge
    }
}
