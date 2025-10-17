package com.example.elk

import com.example.elk.models.InputGraph
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.types.file
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import java.io.File
import kotlin.system.exitProcess

class GraphLayoutApp : CliktCommand(name = "elk-layout") {
    
    private val inputFile by option("-i", "--input", help = "Input JSON file")
        .file(mustExist = true, canBeDir = false, mustBeReadable = true)
    
    private val outputFile by option("-o", "--output", help = "Output JSON file")
        .file(canBeDir = false)
    
    private val pretty by option("--pretty", help = "Pretty print JSON output")
        .default("false")
    
    private val json = Json {
        prettyPrint = true
        ignoreUnknownKeys = true
    }
    
    override fun run() {
        // Read input
        val inputText = if (inputFile != null) {
            echo("Reading input file $inputFile")
            inputFile!!.readText()
        } else {
            // Read from stdin
            generateSequence(::readLine).joinToString("\n")
        }

        // Parse input JSON
        val inputGraph = json.decodeFromString<InputGraph>(inputText)

        // Process graph
        echo("Processing graph with ${inputGraph.nodes.size} nodes and ${inputGraph.edges.size} edges...")
        val processor = GraphProcessor()
        val outputGraph = processor.processGraph(inputGraph)

        // Serialize output
        val outputJson = if (pretty == "true") {
            json.encodeToString(outputGraph)
        } else {
            Json.encodeToString(outputGraph)
        }

        // Write output
        if (outputFile != null) {
            outputFile!!.writeText(outputJson)
            echo("Layout completed! Output written to ${outputFile!!.absolutePath}")
        } else {
            println(outputJson)
        }

        // Print summary
        echo("Layout summary:")
        echo("  - Execution time: ${outputGraph.layoutInfo.executionTime}ms")
        echo("  - Graph bounds: ${outputGraph.bounds.width} x ${outputGraph.bounds.height}")
        echo("  - Algorithm: ${outputGraph.layoutInfo.algorithm}")
        echo("  - Direction: ${outputGraph.layoutInfo.direction}")

    }
}

fun main(args: Array<String>) = GraphLayoutApp().main(args)
