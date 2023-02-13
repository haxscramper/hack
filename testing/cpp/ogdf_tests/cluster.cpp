#include <ogdf/basic/Graph.h>
#include <ogdf/basic/graph_generators.h>
#include <ogdf/fileformats/GraphIO.h>
#include <ogdf/basic/LayoutModule.h>
#include <ogdf/cluster/ClusterPlanarizationLayout.h>
#include <ogdf/cluster/ClusterOrthoLayout.h>

#include <ogdf/planarlayout/MixedModelLayout.h>
#include <ogdf/fileformats/GraphIO.h>

using namespace ogdf;

// The example works, but compilation must be done using cmake
// -DOGDF_MEMORY_MANAGER=MALLOC_TS ..
// https://github.com/ogdf/ogdf/issues/83
void generateExampleGraph() {
    Graph                  G;
    ClusterGraph           CG(G);
    ClusterGraphAttributes GA(CG, ClusterGraphAttributes::all);
    GraphIO::read(GA, CG, G, "infile.gml");
    ClusterPlanarizationLayout cpl;
    cpl.call(G, GA, CG, true);
    GraphIO::write(GA, "cluster-out.tmp.svg");
    std::cout << "wrote GML-based cluster graph" << std::endl;
}

int main() {
    generateExampleGraph();

    for (int i = 0; i < 2; ++i) {
        Graph        G;
        ClusterGraph C;

        C.init(G);

        SList<node> nodes;
        auto        first  = G.newNode();
        auto        second = G.newNode();
        auto        third  = G.newNode();

        nodes.pushBack(first);
        C.createCluster(nodes);
        nodes.clear();
        nodes.pushBack(second);
        nodes.pushBack(third);
        C.createCluster(nodes);
        G.newEdge(first, second);
        G.newEdge(second, third);

        if (i == 0) {
            GraphAttributes  GA(G);
            MixedModelLayout MML;
            MML.call(GA);
            std::cout << "done execution of mixed model layout"
                      << std::endl;
            GraphIO::drawSVG(GA, "cluster-mixed-model.tmp.svg");
        }

        if (i == 1) {
            ClusterGraphAttributes     cAttr(C);
            ClusterPlanarizationLayout clusterPlanarizationLayout;
            clusterPlanarizationLayout.call(G, cAttr, C);
            std::cout << "done execution" << std::endl;
            GraphIO::drawSVG(cAttr, "cluster.tmp.svg");
        }
    }
}
