#include <ogdf/basic/Graph.h>
#include <ogdf/basic/graph_generators.h>
#include <ogdf/fileformats/GraphIO.h>


using namespace ogdf;


GraphAttributes enumerateGraphContent(Graph const& G) {
    GraphAttributes GA(
        G,
        GraphAttributes::nodeGraphics | GraphAttributes::edgeGraphics
            | GraphAttributes::nodeLabel | GraphAttributes::nodeStyle
            | GraphAttributes::edgeType | GraphAttributes::edgeArrow
            | GraphAttributes::edgeStyle); // Create graph attributes
                                           // for this graph

    for (node v : G.nodes) { // iterate through all the node in the
                             // graph
        GA.fillColor(v) = Color("#FFFF00"); // set node color to
                                            // yellow

        GA.height(v) = 20.0; // set the height to 20.0
        GA.width(v)  = 20.0; // set the width to 40.0
        GA.shape(v)  = ogdf::Shape::Ellipse;

        string      s     = to_string(v->index());
        char const* pchar = s.c_str(); // use char const* as target
                                       // type
        GA.label(v) = pchar;
    }

    for (edge e : G.edges) // set default edge color and type
    {
        GA.bends(e);
        // GA.arrowType(e) = ogdf::EdgeArrow::eaNone;
        GA.strokeColor(e) = Color("#0000FF");
    }

    return GA;
}
