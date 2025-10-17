package com.example.elk

import com.example.elk.models.*
import org.eclipse.elk.core.RecursiveGraphLayoutEngine
import org.eclipse.elk.core.util.NullElkProgressMonitor
import org.eclipse.elk.graph.ElkEdge
import org.eclipse.elk.graph.ElkNode

class GraphProcessor {
    private val elkGraphBuilder = ElkGraphBuilder()
    private val layoutEngine = RecursiveGraphLayoutEngine()
    
    fun processGraph(inputGraph: InputGraph): OutputGraph {
        val startTime = System.currentTimeMillis()
        
        // Build ELK graph
        val elkGraph = elkGraphBuilder.buildElkGraph(inputGraph)
        
        // Apply layout
        layoutEngine.layout(elkGraph, NullElkProgressMonitor())
        
        val endTime = System.currentTimeMillis()
        val executionTime = endTime - startTime
        
        // Convert back to output format
        return convertToOutputGraph(elkGraph, inputGraph, executionTime)
    }
    
    private fun convertToOutputGraph(
        elkGraph: ElkNode, 
        inputGraph: InputGraph,
        executionTime: Long
    ): OutputGraph {
        // Convert nodes
        val outputNodes = elkGraph.children.mapIndexed { index, elkNode ->
            val inputNode = inputGraph.nodes[index]
            OutputNode(
                id = inputNode.id,
                label = inputNode.label,
                position = Position(elkNode.x, elkNode.y),
                size = Size(elkNode.width, elkNode.height),
                properties = inputNode.properties
            )
        }
        
        // Convert edges
        val outputEdges = elkGraph.containedEdges.mapIndexed { index, elkEdge ->
            val inputEdge = inputGraph.edges.getOrNull(index)
            val sourceIds = elkEdge.sources.mapNotNull { findNodeId(it, inputGraph) }
            val targetIds = elkEdge.targets.mapNotNull { findNodeId(it, inputGraph) }

            OutputEdge(
                id = inputEdge?.id,
                sources = sourceIds,
                targets = targetIds,
                label = inputEdge?.label,
                points = extractEdgePoints(elkEdge),
                properties = inputEdge?.properties ?: emptyMap()
            )
        }
        
        // Graph bounds
        val bounds = GraphBounds(
            x = elkGraph.x,
            y = elkGraph.y,
            width = elkGraph.width,
            height = elkGraph.height
        )
        
        // Layout info
        val layoutInfo = LayoutInfo(
            algorithm = "layered",
            direction = inputGraph.layoutOptions?.direction ?: "DOWN",
            executionTime = executionTime,
            nodeCount = outputNodes.size,
            edgeCount = outputEdges.size
        )
        
        return OutputGraph(outputNodes, outputEdges, bounds, layoutInfo)
    }
    
    private fun findNodeId(elkNode: Any?, inputGraph: InputGraph): String? {
        if (elkNode !is ElkNode) return null
        
        val index = (elkNode.parent as? ElkNode)?.children?.indexOf(elkNode) ?: -1
        return if (index >= 0 && index < inputGraph.nodes.size) {
            inputGraph.nodes[index].id
        } else null
    }
    
    private fun extractEdgePoints(elkEdge: ElkEdge): List<Position> {
        val points = mutableListOf<Position>()
        
        // Add source point
        elkEdge.sources.firstOrNull()?.let { source ->
            if (source is ElkNode) {
                points.add(Position(
                    source.x + source.width / 2,
                    source.y + source.height / 2
                ))
            }
        }
        
        // Add bend points if any
        elkEdge.sections.forEach { section ->
            section.bendPoints.forEach { bendPoint ->
                points.add(Position(bendPoint.x, bendPoint.y))
            }
        }
        
        // Add target point
        elkEdge.targets.firstOrNull()?.let { target ->
            if (target is ElkNode) {
                points.add(Position(
                    target.x + target.width / 2,
                    target.y + target.height / 2
                ))
            }
        }
        
        return points
    }
}
