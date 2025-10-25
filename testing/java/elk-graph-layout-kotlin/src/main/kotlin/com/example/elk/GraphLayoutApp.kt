package com.example.elk

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.types.file
import org.eclipse.elk.core.RecursiveGraphLayoutEngine
import org.eclipse.elk.core.options.CoreOptions
import org.eclipse.elk.core.util.BasicProgressMonitor
import org.eclipse.elk.graph.ElkNode
import org.eclipse.elk.graph.json.ElkGraphJson
import org.eclipse.elk.graph.json.JsonImporter
import org.eclipse.elk.alg.layered.options.LayeredOptions
import com.google.gson.JsonObject
import com.google.gson.JsonParser
import org.eclipse.elk.core.util.Maybe
import java.io.File

class GraphLayoutApp : CliktCommand(name = "elk-layout") {

    private val inputFile by option("-i", "--input", help = "Input JSON file")
        .file(mustExist = true, canBeDir = false, mustBeReadable = true)

    private val outputFile by option("-o", "--output", help = "Output JSON file")
        .file(canBeDir = false)

    private val pretty by option("--pretty", help = "Pretty print JSON output")
        .default("false")

    override fun run() {
        val inputText: String = if (inputFile != null) {
            echo("Reading input file $inputFile")
            inputFile!!.readText()
        } else {
            generateSequence(::readLine).joinToString("\n")
        }

        val jsonGraph: JsonObject = JsonParser.parseString(inputText).asJsonObject
        val elkGraph: ElkNode = ElkGraphJson.forGraph(jsonGraph)
            .toElk()

        echo("Processing graph with ${elkGraph.children.size} nodes and ${elkGraph.containedEdges.size} edges...")

        val layoutEngine: RecursiveGraphLayoutEngine = RecursiveGraphLayoutEngine()
        val monitor: BasicProgressMonitor = BasicProgressMonitor()

        val startTime: Long = System.currentTimeMillis()
        layoutEngine.layout(elkGraph, monitor)
        val executionTime: Long = System.currentTimeMillis() - startTime
        val outputJson: String = ElkGraphJson.forGraph(elkGraph).prettyPrint(true).toJson()

        if (outputFile != null) {
            outputFile!!.writeText(outputJson)
            echo("Layout completed! Output written to ${outputFile!!.absolutePath}")
        } else {
            println(outputJson)
        }

        val algorithm: String = elkGraph.getProperty(CoreOptions.ALGORITHM) ?: "layered"
        val direction: String = elkGraph.getProperty(CoreOptions.DIRECTION)?.toString() ?: "UNDEFINED"

        echo("Layout summary:")
        echo("  - Execution time: ${executionTime}ms")
        echo("  - Graph bounds: ${elkGraph.width} x ${elkGraph.height}")
        echo("  - Algorithm: ${algorithm}")
        echo("  - Direction: ${direction}")
    }
}

fun main(args: Array<String>) = GraphLayoutApp().main(args)
