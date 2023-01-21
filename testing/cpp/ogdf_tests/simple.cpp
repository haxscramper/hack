#include "test_common.hpp"

#include <ogdf/layered/DfsAcyclicSubgraph.h>
#include <ogdf/layered/SugiyamaLayout.h>
#include <ogdf/layered/OptimalRanking.h>
#include <ogdf/layered/MedianHeuristic.h>
#include <ogdf/layered/OptimalHierarchyLayout.h>

#include <ogdf/planarlayout/MixedModelLayout.h>
#include <ogdf/planarlayout/PlanarStraightLayout.h>
#include <ogdf/tree/TreeLayout.h>
#include <ogdf/planarlayout/PlanarDrawLayout.h>
#include <ogdf/planarity/PlanarizationGridLayout.h>
#include <ogdf/energybased/FMMMLayout.h>
#include <ogdf/tree/RadialTreeLayout.h>
#include <ogdf/misclayout/CircularLayout.h>
#include <ogdf/misclayout/BalloonLayout.h>

#include <vector>
#include <utility>
#include <string>

using namespace ogdf;

int main() {

    const std::vector<std::string> configurations{
        "random_simple",
        "balloon",
        "circular",
        "tree",
        "radial_tree",
        "planar_straight",
        "mixed_model",
        "fmmm",
        "planar_draw",
    };

    for (const auto conf : configurations) {
        std::cout << "running: " << conf << std::endl;
        Graph G;

        if (conf == "circular") {
            wheelGraph(G, 15);
        }

        if (conf == "balloon") {
            wheelGraph(G, 15);
        }

        if (conf == "radial_tree") {
            randomTree(G, 10);
        }

        if (conf == "tree") {
            randomTree(G, 10);
        }

        if (conf == "fmmm") {
            randomGraph(G, 12, 23);
        }

        if (conf == "planarization_grid") {
            randomTriconnectedGraph(G, 10, 2, 3);
        }

        if (conf == "planar_draw") {
            randomTriconnectedGraph(G, 10, 2, 3);
        }

        if (conf == "random_simple") {
            randomGraph(G, 10, 35); // grapth genarate.
        }

        if (conf == "mixed_model") {
            randomTriconnectedGraph(G, 10, 1, 2);
        }

        if (conf == "planar_straight") {
            randomTriconnectedGraph(G, 10, 2, 3);
        }

        GraphAttributes GA = enumerateGraphContent(G);
        if (conf == "random_simple") {
            SugiyamaLayout SL; // Compute a hierarchical drawing of G
                               // (using SugiyamaLayout)
            SL.setRanking(new OptimalRanking());
            SL.setCrossMin(new MedianHeuristic());

            OptimalHierarchyLayout* ohl = new OptimalHierarchyLayout();

            SL.setLayout(ohl);
            SL.call(GA);
        }

        if (conf == "tree") {
            TreeLayout TL;
            TL.call(GA);
        }

        if (conf == "mixed_model") {
            MixedModelLayout MML;
            MML.call(GA);
        }

        if (conf == "planar_straight") {
            PlanarStraightLayout PSL;
            PSL.call(GA);
        }

        if (conf == "planar_draw") {
            PlanarDrawLayout PDL;
            PDL.call(GA);
        }

        if (conf == "planarization_grid") {
            PlanarizationGridLayout PGL;
            PGL.call(GA);
        }

        if (conf == "fmmm") {
            FMMMLayout FL;
            FL.call(GA);
        }

        if (conf == "radial_tree") {
            RadialTreeLayout RTL;
            RTL.call(GA);
        }

        if (conf == "circular") {
            CircularLayout CL;
            CL.call(GA);
        }

        if (conf == "balloon") {
            BalloonLayout BL;
            BL.call(GA);
        }

        std::cout << ":> " << conf << std::endl;
        GraphIO::drawSVG(GA, "simple_out_svg" + conf + ".tmp.svg");
    }

    std::cout << "done\n";
    return 0;
}
