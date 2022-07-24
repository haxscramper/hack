#include <ogdf/basic/graph_generators.h>
#include <ogdf/layered/DfsAcyclicSubgraph.h>
#include <ogdf/fileformats/GraphIO.h>
#include <ogdf/fileformats/GraphIO.h>
#include <ogdf/layered/MedianHeuristic.h>
#include <ogdf/layered/OptimalHierarchyLayout.h>
#include <ogdf/layered/OptimalRanking.h>
#include <ogdf/layered/SugiyamaLayout.h>
#include <ogdf/orthogonal/OrthoLayout.h>

#include <stdlib.h>
#include <time.h>

using namespace ogdf;

int main() {
    srand(time(NULL));

    Graph G;
    randomSimpleGraph(G, 20, 40);

    GraphAttributes GA(
        G,
        GraphAttributes::nodeGraphics | GraphAttributes::edgeGraphics |
            GraphAttributes::nodeLabel | GraphAttributes::edgeStyle |
            GraphAttributes::edgeLabel | GraphAttributes::nodeStyle |
            GraphAttributes::nodeTemplate);

    int idx = 0;
    for (auto gnode : G.nodes) {
        GA.label(gnode) = std::to_string(idx);
        ++idx;
    }


    if (true) {
        SugiyamaLayout SL;
        SL.setRanking(new OptimalRanking);
        SL.setCrossMin(new MedianHeuristic);

        OptimalHierarchyLayout* ohl = new OptimalHierarchyLayout;
        ohl->layerDistance(30.0);
        ohl->nodeDistance(25.0);
        ohl->weightBalancing(0.8);
        SL.setLayout(ohl);

        SL.call(GA);
    }

    // {
    //     OrthoLayout SL;
    //     SL.call(GA);
    // }

    GraphIO::write(GA, "out-graph.svg", GraphIO::drawSVG);

    return 0;
}
