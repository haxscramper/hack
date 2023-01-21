/// Read `dot_test.dot` file and genertate all possible layouts for it

#include <ogdf/layered/DfsAcyclicSubgraph.h>
#include <ogdf/layered/SugiyamaLayout.h>
#include <ogdf/layered/OptimalRanking.h>
#include <ogdf/layered/MedianHeuristic.h>
#include <ogdf/layered/OptimalHierarchyLayout.h>
#include <ogdf/basic/Graph.h>
#include <ogdf/basic/graph_generators.h>
#include <ogdf/fileformats/GraphIO.h>
#include <ogdf/planarlayout/MixedModelLayout.h>
#include <ogdf/planarlayout/PlanarStraightLayout.h>
#include <ogdf/tree/TreeLayout.h>
#include <ogdf/planarlayout/PlanarDrawLayout.h>
#include <ogdf/planarity/PlanarizationGridLayout.h>
#include <ogdf/energybased/FMMMLayout.h>
#include <ogdf/tree/RadialTreeLayout.h>
#include <ogdf/misclayout/CircularLayout.h>
#include <ogdf/misclayout/BalloonLayout.h>
#include <ogdf/layered/FastHierarchyLayout.h>
#include <ogdf/layered/FastSimpleHierarchyLayout.h>
#include <ogdf/layered/LongestPathRanking.h>
#include <ogdf/upward/LayerBasedUPRLayout.h>
#include <ogdf/energybased/GEMLayout.h>
#include <ogdf/layered/CoffmanGrahamRanking.h>
#include <ogdf/layered/LayerByLayerSweep.h>
#include <ogdf/layered/GridSifting.h>
#include <ogdf/layered/SplitHeuristic.h>
#include <ogdf/simultaneous/TwoLayerCrossMinSimDraw.h>
#include <ogdf/layered/SiftingHeuristic.h>
#include <ogdf/layered/MedianHeuristic.h>
#include <ogdf/layered/GreedySwitchHeuristic.h>
#include <ogdf/layered/GreedyInsertHeuristic.h>
#include <ogdf/layered/BarycenterHeuristic.h>

#include <ogdf/energybased/DavidsonHarelLayout.h>
#include <ogdf/energybased/FastMultipoleEmbedder.h>
#include <ogdf/energybased/FMMMLayout.h>
#include <ogdf/energybased/GEMLayout.h>
#include <ogdf/energybased/MultilevelLayout.h>
#include <ogdf/energybased/NodeRespecterLayout.h>
#include <ogdf/energybased/PivotMDS.h>
#include <ogdf/energybased/SpringEmbedderGridVariant.h>
#include <ogdf/energybased/SpringEmbedderKK.h>
#include <ogdf/energybased/StressMinimization.h>
#include <ogdf/energybased/TutteLayout.h>


#include <vector>
#include <utility>
#include <string>
#include <fstream>

using namespace ogdf;


void enumerateGraphContent(GraphAttributes& GA, Graph const& G) {
    for (node v : G.nodes) {
        GA.fillColor(v) = Color("#FFFF00");
        GA.height(v)    = 20.0;
        GA.width(v)     = 20.0;

        string      s     = to_string(v->index());
        char const* pchar = s.c_str();
        GA.label(v)       = pchar;
    }

    for (edge e : G.edges) {
        GA.bends(e);
        GA.strokeColor(e) = Color("#0000FF");
    }
}

template <typename T>
using StrVec = std::vector<std::pair<std::string, T>>;

int main() {
    StrVec<std::function<void(GraphAttributes&, Graph&)>> configurations{
        // {"tree-layout",
        //  [](GraphAttributes& GA, Graph& G) {
        //      // randomTree(G, 10);
        //      TreeLayout TL;
        //      TL.call(GA);
        //  }},
        // {"radial-tree",
        //  [](GraphAttributes& GA, Graph& G) {
        //      // randomTree(G, 10);
        //      RadialTreeLayout RTL;
        //      RTL.call(GA);
        //  }},
        {"", {[](GraphAttributes& GA, Graph& G) {}}},
        {"balloon", {[](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             BalloonLayout BL;
             BL.call(GA);
         }}},
        {"planarization_grid", {[](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             PlanarizationGridLayout PGL;
             PGL.call(GA);
         }}},
        {"mixed_model", {[](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             MixedModelLayout MML;
             MML.call(GA);
         }}},
        {"planar_straight", {[](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             PlanarStraightLayout PSL;
             PSL.call(GA);
         }}},
        {"DavidsonHarelLayout",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             DavidsonHarelLayout lyt;
             lyt.call(GA);
         }},
        {"FastMultipoleEmbedder",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             FastMultipoleEmbedder lyt;
             lyt.call(GA);
         }},
        {"FastMultipoleMultilevelEmbedder",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             FastMultipoleMultilevelEmbedder lyt;
             lyt.call(GA);
         }},
        {"FMMMLayout",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             FMMMLayout lyt;
             lyt.call(GA);
         }},
        {"GEMLayout",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             GEMLayout lyt;
             lyt.call(GA);
         }},
        {"MultilevelLayout",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             MultilevelLayout lyt;
             lyt.call(GA);
         }},
        {"NodeRespecterLayouut",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             NodeRespecterLayout lyt;
             lyt.call(GA);
         }},
        {"PivotMDS",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             PivotMDS lyt;
             lyt.call(GA);
         }},
        {"SpringEmbedderGridVariant",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             SpringEmbedderGridVariant lyt;
             lyt.call(GA);
         }},
        {"SpringEmbedderKK",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             SpringEmbedderKK lyt;
             lyt.call(GA);
         }},
        {"StressMinimization",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             StressMinimization lyt;
             lyt.call(GA);
         }},
        {"TutteLayot",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             TutteLayout lyt;
             lyt.call(GA);
         }},
    };


    for (const auto& ranking : StrVec<std::function<RankingModule*()>>{{
             // clang-format off
             {"optimal", []() { return new OptimalRanking(); }},
             {"coffman_graham", []() { return new CoffmanGrahamRanking(); }},
             {"longest_path", []() { return new LongestPathRanking(); }},
             // clang-format on
         }}) {
        for (const auto& hierarchy :
             StrVec<std::function<HierarchyLayoutModule*()>>{{
                 // clang-format off
                 {"optimal", []() { return new OptimalHierarchyLayout(); }},
                 {"fast", []() { return new FastHierarchyLayout(); }},
                 {"fast_simple", []() { return new FastSimpleHierarchyLayout(); }},
                 // clang-format on
             }}) {

            // NOTE Don't understand how to use this one, `UpwardPlanRep &
            // UPR` parameter in the layout specifically
            //
            // configurations.push_back(
            //     {"upr" + ranking.first + "_ranking" + hierarchy.first +
            //     "_hierarchy",
            //      [ranking, hierarchy](
            //          GraphAttributes& GA, Graph& G) {
            //          LayerBasedUPRLayout UPR;
            //          UPR.setRanking(ranking.second());
            //          UPR.setLayout(hierarchy.second());
            //          UPR.call(GA);
            //      }});

            for (const auto& cross :
                 StrVec<std::function<LayeredCrossMinModule*()>>{{
                     // clang-format off
                     {"median_heuristic", []() { return new MedianHeuristic(); }},
                     {"grid_sifting", []() { return new GridSifting(); }},
                     {"split", []() { return new SplitHeuristic(); }},
                     {"sifting", []() { return new SiftingHeuristic(); }},
                     {"greedy_switch", []() { return new GreedySwitchHeuristic(); }},
                     {"greedy_insert", []() { return new GreedyInsertHeuristic(); }},
                     {"barycentric", []() { return new BarycenterHeuristic(); }}
                     // clang-format on
                 }}) {

                configurations.push_back(
                    {"sugiyama" + ranking.first + "_ranking" + cross.first
                         + "_cross" + hierarchy.first + "_hierarchy",
                     [ranking, cross, hierarchy](
                         GraphAttributes& GA, Graph& G) {
                         SugiyamaLayout SL;
                         SL.setRanking(ranking.second());
                         SL.setCrossMin(cross.second());
                         SL.setLayout(hierarchy.second());
                         SL.call(GA);
                     }});
            }
        }
    }

    for (const auto& conf : configurations) {
        std::cout << "running: " << conf.first << std::endl;
        std::cout << "Reading dot file configuration" << std::endl;
        std::ifstream file{"dot_test.dot"};
        Graph         G;
        GraphIO::readDOT(G, file);
        std::cout << "READ DONE " << std::endl;

        GraphAttributes GA(
            G,
            GraphAttributes::nodeGraphics | GraphAttributes::edgeGraphics
                | GraphAttributes::nodeLabel | GraphAttributes::nodeStyle
                | GraphAttributes::edgeType | GraphAttributes::edgeArrow
                | GraphAttributes::edgeStyle);
        std::cout << "CALL LAYOUT" << std::endl;
        try {
            conf.second(GA, G);
            std::cout << ":> " << conf.first << std::endl;
            GraphIO::drawSVG(GA, "dot_" + conf.first + ".tmp.svg");
        } catch (std::exception& ex) {
            std::cerr << "Layout failed " << ex.what() << std::endl;
        }
    }

    std::cout << "DONE ____________ !\n";
    return 0;
}
