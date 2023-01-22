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
#include <ogdf/upward/UpwardPlanarizationLayout.h>
#include <ogdf/upward/DominanceLayout.h>
#include <ogdf/upward/VisibilityLayout.h>

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

#include <ogdf/planarlayout/MMCBDoubleGrid.h>
#include <ogdf/planarlayout/MMCBLocalStretch.h>

#include <ogdf/augmentation/PlanarAugmentationFix.h>
#include <ogdf/augmentation/PlanarAugmentation.h>
#include <ogdf/augmentation/DfsMakeBiconnected.h>

#include <vector>
#include <utility>
#include <string>
#include <fstream>

#include <ogdf/planarlayout/TriconnectedShellingOrder.h>
#include <ogdf/planarlayout/BiconnectedShellingOrder.h>

using namespace ogdf;

bool contains(std::string const& str, std::string const& sub) {
    return str.find(sub) != std::string::npos;
}

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
        {"UpwardPlanarizationLayout",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             UpwardPlanarizationLayout lyt;
             lyt.call(GA);
         }},
        {"VisibilityLayout",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             VisibilityLayout lyt;
             lyt.call(GA);
         }},
        {"DominanceLayout",
         [](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             DominanceLayout lyt;
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

    // NOTE setting augmentation module or shelling order module on these
    // layouts leads to unbounded memory bugs for almost all graphs.
    // Default configuration works file though.
    configurations.push_back(
        {"planar_straight_" // + conf
         ,
         {[=](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             PlanarStraightLayout PSL;
             // PSL.setShellingOrder(shelling.second());
             // PSL.setAugmenter(augmenter.second());
             PSL.call(GA);
         }}});

    configurations.push_back(
        {"planar_draw_" // + conf
         ,
         {[=](GraphAttributes& GA, Graph& G) {
             enumerateGraphContent(GA, G);
             PlanarDrawLayout PSL;
             // PSL.setShellingOrder(shelling.second());
             // PSL.setAugmenter(augmenter.second());
             PSL.call(GA);
         }}});


    for (const auto& shelling :
         StrVec<std::function<ShellingOrderModule*()>>{{
             // clang-format off
            {"biconnect", [](){ return new BiconnectedShellingOrder(); }},
            {"triconnect", [](){ return new TriconnectedShellingOrder(); }},
             // clang-format on
         }}) {


        for (const auto& augmenter :
             StrVec<std::function<AugmentationModule*()>>{{
                 // clang-format off
                 {"planar_fix", [](){ return new PlanarAugmentationFix(); }},
                 {"planar", [](){ return new PlanarAugmentation(); }},
                 {"dfs", [](){ return new DfsMakeBiconnected(); }},
                 // clang-format on
             }}) {

            const auto conf = "shelling=" + shelling.first
                            + ",augmenter=" + augmenter.first;


            for (const auto& cross : StrVec<std::function<
                     MixedModelCrossingsBeautifierModule*()>>{{
                     // clang-format off
                     {"double_grid", [](){return new MMCBDoubleGrid();}},
                     {"local_stretch", [](){return new MMCBLocalStretch();}},
                     // clang-format on
                 }}) {
                configurations.push_back(
                    {"mixed_model_cross=" + cross.first + "," + conf,
                     {[=](GraphAttributes& GA, Graph& G) {
                         enumerateGraphContent(GA, G);
                         MixedModelLayout MML;
                         MML.setCrossingsBeautifier(cross.second());
                         MML.setShellingOrder(shelling.second());
                         MML.setAugmenter(augmenter.second());
                         MML.call(GA);
                     }}});
            }
        }
    }


    for (const auto& quality : StrVec<FMMMOptions::QualityVsSpeed>{{
             // clang-format off
             {"gae", FMMMOptions::QualityVsSpeed::GorgeousAndEfficient},
             {"nais", FMMMOptions::QualityVsSpeed::NiceAndIncredibleSpeed},
             // clang-format on
         }}) {
        for (const auto& forces :
             StrVec<FMMMOptions::RepulsiveForcesMethod>{{
                 // clang-format off
                 {"grid", FMMMOptions::RepulsiveForcesMethod::GridApproximation},
                 {"nmm", FMMMOptions::RepulsiveForcesMethod::NMM},
                 // clang-format on
             }}) {
            configurations.push_back(
                {"FMMMLayout_quality=" + quality.first
                     + ",forces=" + forces.first,
                 [=](GraphAttributes& GA, Graph& G) {
                     enumerateGraphContent(GA, G);
                     FMMMLayout lyt;
                     lyt.repulsiveForcesCalculation(forces.second);
                     lyt.qualityVersusSpeed(quality.second);
                     lyt.call(GA);
                 }});
        }
    }


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
                    {"sugiyama_ranking=" + ranking.first + ",cross="
                         + cross.first + ",hierarchy=" + hierarchy.first,
                     [ranking, cross, hierarchy](
                         GraphAttributes& GA, Graph& G) {
                         enumerateGraphContent(GA, G);
                         SugiyamaLayout SL;
                         SL.setRanking(ranking.second());
                         SL.setCrossMin(cross.second());
                         SL.setLayout(hierarchy.second());
                         SL.call(GA);
                     }});
            }
        }
    }

    const std::vector<std::string> files{
        "dot_test.dot",
        "go_packages.dot",
        "ninja_build.dot",
    };

    for (const auto& infile : files) {
        std::cout << "file: " << infile << std::endl;
        for (const auto& conf : configurations) {
            const auto n = conf.first;
            bool       skip
                = (((
                        // Infinite execution
                        contains(n, "mixed_model")
                        || contains(n, "planar_straight")
                        || contains(n, "planar_draw")
                        // Uncontrolled memory allocation
                        || contains(n, "TutteLayot"))
                    && infile == "go_packages.dot")
                   || ((
                           // Memory bugs
                           (contains(n, "mixed_model")
                            && (contains(n, "triconnect")
                                || contains(n, "augmenter=planar")))
                           || ((contains(n, "planar_straight")
                                || contains(n, "planar_draw"))
                               && (contains(n, "augmenter=planar_fix")
                                   || contains(n, "augmenter=planar")
                                   || contains(n, "augmenter=dfs"))))
                       && infile == "dot_test.dot")
                   || ((
                           // Memory bug
                           (contains(n, "mixed_model")
                            && (contains(n, "triconnect")
                                || contains(n, "augmenter=planar"))))
                       && infile == "ninja_build.dot"));

            if (skip) {
                std::cout << "  skipping " << n << std::endl;
            } else {
                std::cout << "  " << n << std::endl;
                std::ifstream   file{infile};
                Graph           G;
                GraphAttributes GA(
                    G,
                    GraphAttributes::nodeGraphics
                        | GraphAttributes::edgeGraphics
                        | GraphAttributes::nodeLabel
                        | GraphAttributes::nodeStyle
                        | GraphAttributes::edgeType
                        | GraphAttributes::edgeArrow
                        | GraphAttributes::edgeStyle);

                GraphIO::readDOT(GA, G, file);


                try {
                    conf.second(GA, G);
                    GraphIO::drawSVG(
                        GA, "/tmp/dot_" + infile + n + ".tmp.svg");
                } catch (std::exception& ex) {
                    std::cerr << "Layout failed " << ex.what()
                              << std::endl;
                }
            }
        }
    }


    std::cout << "DONE ____________ !\n";
    return 0;
}
