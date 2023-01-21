#include <ogdf/basic/Graph.h>
#include <ogdf/basic/graph_generators.h>
#include <ogdf/basic/simple_graph_alg.h>
#include <ogdf/fileformats/GraphIO.h>
#include <ogdf/basic/LayoutModule.h>
#include <ogdf/cluster/ClusterPlanarizationLayout.h>
#include <ogdf/cluster/ClusterOrthoLayout.h>

#include <ogdf/basic/LayoutModule.h>
#include <ogdf/cluster/ClusterPlanarizationLayout.h>
#include <ogdf/cluster/ClusterOrthoLayout.h>

#include <ogdf/fileformats/GraphIO.h>

#include <set>

using namespace ogdf;

inline void splitParallelEdges(Graph& G) {
    List<edge> edges;
    G.allEdges(edges);

    for (edge e : edges) {
        for (adjEntry adj : e->source()->adjEntries) {
            if (adj->twinNode() == e->target() && adj->theEdge() != e) {
                G.split(e);
            }
        }
    }
}

inline void createDisconnectedGraph(
    Graph& G,
    int    nMax,
    double densityMin,
    double densityMax,
    int    cc,
    int    bc) {
    OGDF_ASSERT(cc > 0);
    OGDF_ASSERT(bc > 0);
    OGDF_ASSERT(densityMin > 0);
    OGDF_ASSERT(densityMax >= densityMin);
    OGDF_ASSERT(densityMax < 3);

    G.clear();

    int nBcMax = ceil(nMax / (cc * bc));
    nBcMax     = std::max(nBcMax, 2);

    for (int i = 0; i < cc; i++) {
        int m = ceil(
            randomDouble(densityMin * nBcMax, densityMax * nBcMax));
        Graph H;
        randomPlanarCNBGraph(H, nBcMax, m, bc);
        G.insert(H);
    }
}

inline void createAlmostPlanarGraph(Graph& G, int n, int m, int add_m) {
    randomPlanarBiconnectedGraph(G, n, m);

    Array<node> table(n);

    int i = 0;
    for (node v : G.nodes) {
        OGDF_ASSERT(i < n);
        table[i++] = v;
    }

    for (i = 0; i < add_m; ++i) {
        G.newEdge(
            table[randomNumber(0, n - 1)], table[randomNumber(0, n - 1)]);
    }

    makeSimpleUndirected(G);
}


inline void addMultiEdges(Graph& G, double p) {
    OGDF_ASSERT(p >= 0);
    OGDF_ASSERT(p < 1);

    auto byChance = [&]() -> bool { return randomDouble(0, 1) < p; };

    List<edge> edges;
    G.allEdges(edges);

    for (node v : G.nodes) {
        while (byChance()) {
            G.newEdge(v, v);
        }
    }

    for (edge e : edges) {
        node v = e->source();
        node w = e->target();

        while (byChance()) {
            G.newEdge(v, w);

            if (byChance()) {
                std::swap(v, w);
            }
        }
    }
}

class GraphSizes {
    const int m_min;
    const int m_max;
    const int m_step;

  public:
    //! Creates feasible graph sizes ranging from \p min to \p max
    //! with a step size of \p step.
    GraphSizes(int min, int max, int step)
        : m_min(min), m_max(max), m_step(step) {
        OGDF_ASSERT(m_min <= m_max);
        OGDF_ASSERT(m_step > 0);
    }

    //! Default graphs sizes result in 3 iterations
    //! over graphs with at most 100 nodes
    GraphSizes() : GraphSizes(16, 100, 42) {}

    //! Creates just one feasible size that is \p n
    GraphSizes(int n) : GraphSizes(n, n, 1) {}

    //! calls \p func for each feasible graph size
    void forEachSize(std::function<void(int size)> func) const {
        for (int n = m_min; n <= m_max; n += m_step) {
            func(n);
        }
    }
};

enum class GraphProperty
{
    //! Indicates graphs that are (directed!) acyclic.
    acyclic,

    arborescenceForest,
    connected,
    biconnected,
    nonPlanar,
    maxDeg4,
    planar,
    triconnected,

    //! Indicates graphs that are (undirected!) simple.
    simple,
    loopFree,

    //! Indicates instances that have a reasonably low number of edges.
    //! These graphs can, e.g., be used for planarization layouts without
    //! raising runtime too much.
    sparse
};


class CPLMock : public LayoutModule {
    ClusterPlanarizationLayout clusterPlanarizationLayout;

  public:
    virtual void call(GraphAttributes& attr) override {
        GraphCopy              G(attr.constGraph());
        ClusterGraph           C(G);
        ClusterGraphAttributes cAttr(C);
        bool                   originalEmpty = G.numberOfNodes() == 0;

        // add clique cluster
        SList<node> nodes;
        node        nodeInClique = G.newNode();
        nodes.pushBack(nodeInClique);

        for (int i = 0; i < 10; i++) {
            node w = G.newNode();
            for (node v : nodes) {
                G.newEdge(v, w);
            }
            nodes.pushBack(w);
        }
        C.createCluster(nodes, C.firstCluster());

        // add path cluster
        nodes.clear();
        nodes.pushBack(G.newNode());
        for (int i = 0; i < 10; i++) {
            node w = G.newNode();
            G.newEdge(nodes.back(), w);
            nodes.pushBack(w);
        }

        // connect it all
        G.newEdge(nodeInClique, nodes.front());
        G.newEdge(nodeInClique, nodes.back());
        if (!originalEmpty) {
            G.newEdge(nodeInClique, G.firstNode());
        }

        clusterPlanarizationLayout.call(G, cAttr, C);

        for (node v : G.nodes) {
            if (!G.isDummy(v)) {
                attr.x(G.original(v)) = cAttr.x(v);
                attr.y(G.original(v)) = cAttr.y(v);
            }
        }

        for (edge e : G.edges) {
            if (!G.isDummy(e)) {
                attr.bends(G.original(e)) = cAttr.bends(e);
            }
        }
    }
};

//! Returns true if \p subset is a subset of \p superset
inline bool doesInclude(
    const std::set<GraphProperty>& subset,
    const std::set<GraphProperty>& superset) {
    for (auto r : subset) {
        if (superset.find(r) == superset.end()) {
            return false;
        }
    }

    return true;
}

inline void getRandomLayout(GraphAttributes& GA) {
    const Graph& G     = GA.constGraph();
    double       max_x = 2.0 * sqrt(G.numberOfNodes());
    double       max_y = max_x;

    std::minstd_rand                 rng(randomSeed());
    std::uniform_real_distribution<> rand_x(0.0, max_x);
    std::uniform_real_distribution<> rand_y(0.0, max_y);

    for (node v : G.nodes) {
        GA.x(v) = rand_x(rng);
        GA.y(v) = rand_y(rng);
    }
}

inline int64_t callLayout(
    const string& name,
    const Graph&  G,
    LayoutModule& L,
    long          extraAttributes,
    bool          algoPlanarizes,
    bool          algoRequiresPlanar,
    bool          instanceIsPlanar) {
    GraphAttributes GA(
        G,
        extraAttributes | GraphAttributes::nodeGraphics
            | GraphAttributes::nodeStyle | GraphAttributes::edgeGraphics
            | GraphAttributes::edgeStyle);
    getRandomLayout(GA);

    // Initialize the node attributes which are not changed by layout
    // algorithms with random non-default values to make sure the layout
    // algorithm
    // 1. does not fail in this case, and 2. does not reset them to
    // defaults.
    NodeArray<double> widthBefore(G);
    NodeArray<double> heightBefore(G);
    NodeArray<Shape>  shapeBefore(G);
    for (node v : G.nodes) {
        widthBefore[v] = GA.width(v) = randomNumber(1, 20);
        heightBefore[v] = GA.height(v) = randomNumber(1, 20);
        shapeBefore[v] = GA.shape(v) = randomNumber(0, 1) == 0
                                         ? Shape::Pentagon
                                         : Shape::Octagon;
    }

    int64_t result;
    int64_t time;
    System::usedRealTime(time);
    L.call(GA);
    result = System::usedRealTime(time);

#ifdef OGDF_LAYOUT_HELPERS_PRINT_DRAWINGS
    double sumWidths  = 0;
    double sumHeights = 0;

    GA.addAttributes(
        GraphAttributes::nodeLabel | GraphAttributes::edgeArrow);

    for (node v : G.nodes) {
        sumWidths += GA.width(v);
        sumHeights += GA.height(v);

        GA.fillColor(v)   = Color::Name::Red;
        GA.strokeColor(v) = Color::Name::Black;
        GA.label(v)       = to_string(v->index());
    }

    for (edge e : G.edges) {
        GA.strokeWidth(e) = 1;
        GA.strokeColor(e) = Color::Name::Blue;
        GA.arrowType(e)   = EdgeArrow::Last;
    }

    GA.scale(
        sumWidths / GA.boundingBox().width(),
        sumHeights / GA.boundingBox().height(),
        false);
    GA.scale(1.5, false);

    std::regex reg("\\W+");
    string     filename = name;
    std::transform(
        filename.begin(), filename.end(), filename.begin(), ::tolower);
    std::ofstream of(
        "drawing-" + std::regex_replace(filename, reg, "_")
        + "-n=" + to_string(G.numberOfNodes())
        + "-m=" + to_string(G.numberOfEdges()) + "-"
        + to_string(layout_helpers::drawingCounter) + ".svg");
    GraphIO::drawSVG(GA, of);
    layout_helpers::drawingCounter++;
#endif

    std::cout << std::endl;

    // Assert that we do not have any needless bendpoints
    for (edge e : G.edges) {
        DPolyline bends = GA.bends(e);

        int size = bends.size();
        bends.normalize();
    }

    return result;
}

inline void imply(
    std::set<GraphProperty>& props,
    GraphProperty            conclusion,
    GraphProperty            premise) {
    if (doesInclude({premise}, props)) {
        props.insert(conclusion);
    }
};

inline void makeIndicesNonContinuous(Graph& G, double p) {
    OGDF_ASSERT(p >= 0);
    OGDF_ASSERT(p < 1);

    auto byChance = [&]() -> bool { return randomDouble(0, 1) < p; };

#ifdef OGDF_DEBUG
    int n = G.numberOfNodes();
    int m = G.numberOfEdges();
#endif
    List<node> nodes;
    G.allNodes(nodes);

    for (node v : nodes) {
        if (byChance()) {
            // Create gaps before indices for newly inserted node/edges.
            while (byChance()) {
                G.delNode(G.newNode());
            }
            while (byChance()) {
                G.delEdge(G.newEdge(v, v));
            }

            // Create replacement for v.
            node newV = G.newNode();

            // Remember old neighbors.
            ArrayBuffer<node> outNeighbors;
            ArrayBuffer<node> inNeighbors;
            for (adjEntry adj : v->adjEntries) {
                if (adj->theEdge()->isSelfLoop()) {
                    // Remember self-loops only once.
                    if (adj->isSource()) {
                        outNeighbors.push(newV);
                    }
                } else {
                    if (adj->isSource()) {
                        outNeighbors.push(adj->twinNode());
                    } else {
                        inNeighbors.push(adj->twinNode());
                    }
                }
            }

            // Delete v and reinsert incident edges.
            G.delNode(v);
            for (node neighbor : outNeighbors) {
                G.newEdge(newV, neighbor);
            }
            for (node neighbor : inNeighbors) {
                G.newEdge(neighbor, newV);
            }
        }
    }

    OGDF_ASSERT(n == G.numberOfNodes());
    OGDF_ASSERT(m == G.numberOfEdges());
}


inline void performImplications(std::set<GraphProperty>& props) {
    imply(props, GraphProperty::biconnected, GraphProperty::triconnected);
    imply(props, GraphProperty::connected, GraphProperty::biconnected);
    imply(props, GraphProperty::planar, GraphProperty::arborescenceForest);
    imply(
        props, GraphProperty::acyclic, GraphProperty::arborescenceForest);
    imply(props, GraphProperty::loopFree, GraphProperty::simple);

    if (doesInclude({GraphProperty::simple}, props)
        && (doesInclude({GraphProperty::maxDeg4}, props)
            || doesInclude({GraphProperty::planar}, props))) {
        props.insert(GraphProperty::sparse);
    }

    OGDF_ASSERT(!doesInclude(
        {GraphProperty::nonPlanar, GraphProperty::planar}, props));
};


inline void forEachGraphItWorks(
    std::set<GraphProperty>                    requirements,
    std::function<void(
        Graph&,
        const std::string&             graphName,
        const std::set<GraphProperty>& props)> doTest,
    GraphSizes                                 sizes   = GraphSizes(),
    int                                        minSize = 0,
    int  maxSize     = std::numeric_limits<int>::max(),
    bool describable = false) {
    auto testInstance = [&](const string&               desc,
                            std::set<GraphProperty>     props,
                            std::function<void(Graph&)> generateGraph) {
        performImplications(props);

        if (doesInclude(requirements, props)) {
            Graph G;
            generateGraph(G);
            makeIndicesNonContinuous(G, 0.5);

            if (G.numberOfNodes() >= minSize
                && G.numberOfNodes() <= maxSize) {
                doTest(G, desc, props);
            }
        }
    };

    auto testInstances =
        [&](const string&                    desc,
            std::set<GraphProperty>          props,
            std::function<void(Graph&, int)> generateGraph) {
            sizes.forEachSize([&](int n) {
                testInstance(
                    desc + " [n≈" + to_string(n) + "]",
                    props,
                    [&](Graph& G) { generateGraph(G, n); });
            });
        };

    // Single test instances
    // testInstance(
    //     "graph without any nodes",
    //     {GraphProperty::arborescenceForest,
    //      GraphProperty::triconnected,
    //      GraphProperty::maxDeg4,
    //      GraphProperty::acyclic,
    //      GraphProperty::simple},
    //     [](Graph& G) { emptyGraph(G, 0); });

    // testInstance(
    //     "graph with a single node",
    //     {GraphProperty::arborescenceForest,
    //      GraphProperty::triconnected,
    //      GraphProperty::maxDeg4,
    //      GraphProperty::acyclic,
    //      GraphProperty::simple},
    //     [](Graph& G) { emptyGraph(G, 1); });

    testInstance(
        "graph with a single node and one self-loop",
        {GraphProperty::planar,
         GraphProperty::triconnected,
         GraphProperty::maxDeg4,
         GraphProperty::sparse},
        [](Graph& G) {
            customGraph(G, 1, {{0, 0}});
        });

    testInstance(
        "graph with two nodes and no edge",
        {GraphProperty::arborescenceForest,
         GraphProperty::maxDeg4,
         GraphProperty::acyclic,
         GraphProperty::simple},
        [](Graph& G) { emptyGraph(G, 2); });

    testInstance(
        "graph with two nodes and one edge",
        {GraphProperty::arborescenceForest,
         GraphProperty::triconnected,
         GraphProperty::maxDeg4,
         GraphProperty::acyclic,
         GraphProperty::simple},
        [](Graph& G) {
            customGraph(G, 2, {{0, 1}});
        });

    testInstance(
        "graph with two nodes and two edges (one self-loop)",
        {GraphProperty::planar,
         GraphProperty::triconnected,
         GraphProperty::maxDeg4,
         GraphProperty::sparse},
        [](Graph& G) {
            customGraph(G, 2, {{0, 0}, {0, 1}});
        });

    testInstance(
        "graph with two nodes and directed parallel edges",
        {GraphProperty::planar,
         GraphProperty::acyclic,
         GraphProperty::triconnected,
         GraphProperty::maxDeg4,
         GraphProperty::loopFree,
         GraphProperty::sparse},
        [](Graph& G) {
            customGraph(G, 2, {{0, 1}, {0, 1}});
        });

    testInstance(
        "graph with two nodes and undirected parallel edges",
        {GraphProperty::planar,
         GraphProperty::triconnected,
         GraphProperty::maxDeg4,
         GraphProperty::loopFree,
         GraphProperty::sparse},
        [](Graph& G) {
            customGraph(G, 2, {{0, 1}, {1, 0}});
        });

    testInstance(
        "graph with three nodes and no edge",
        {GraphProperty::arborescenceForest,
         GraphProperty::acyclic,
         GraphProperty::maxDeg4,
         GraphProperty::simple},
        [](Graph& G) { emptyGraph(G, 3); });

    testInstance(
        "graph with three nodes and one edge",
        {GraphProperty::arborescenceForest,
         GraphProperty::acyclic,
         GraphProperty::maxDeg4,
         GraphProperty::simple},
        [](Graph& G) {
            customGraph(G, 3, {{0, 1}});
        });

    testInstance(
        "K2,3",
        {GraphProperty::maxDeg4,
         GraphProperty::acyclic,
         GraphProperty::planar,
         GraphProperty::simple,
         GraphProperty::biconnected},
        [](Graph& G) { completeBipartiteGraph(G, 2, 3); });

    testInstance(
        "K3,3",
        {GraphProperty::nonPlanar,
         GraphProperty::maxDeg4,
         GraphProperty::acyclic,
         GraphProperty::simple,
         GraphProperty::triconnected},
        [](Graph& G) { completeBipartiteGraph(G, 3, 3); });

    testInstance(
        "K4",
        {GraphProperty::maxDeg4,
         GraphProperty::planar,
         GraphProperty::simple,
         GraphProperty::acyclic,
         GraphProperty::triconnected},
        [](Graph& G) { completeGraph(G, 4); });

    testInstance(
        "K5",
        {GraphProperty::nonPlanar,
         GraphProperty::maxDeg4,
         GraphProperty::simple,
         GraphProperty::acyclic,
         GraphProperty::triconnected},
        [](Graph& G) { completeGraph(G, 5); });

    testInstance(
        "Petersen graph",
        {GraphProperty::nonPlanar,
         GraphProperty::maxDeg4,
         GraphProperty::triconnected,
         GraphProperty::simple,
         GraphProperty::sparse},
        [](Graph& G) { petersenGraph(G); });

    // testInstance(
    //     "path-like tree",
    //     {GraphProperty::connected,
    //      GraphProperty::planar,
    //      GraphProperty::simple},
    //     [](Graph& G) {
    //         std::stringstream ss{
    //             ResourceFile::get("misc/path-like_tree.gml")->data()};
    //         GraphIO::read(G, ss);
    //     });

    testInstance(
        "non-upward planar graph",
        {GraphProperty::planar,
         GraphProperty::acyclic,
         GraphProperty::simple,
         GraphProperty::sparse,
         GraphProperty::connected},
        [](Graph& G) {
            customGraph(
                G,
                6,
                {{0, 1},
                 {0, 2},
                 {1, 3},
                 {1, 4},
                 {2, 3},
                 {2, 4},
                 {3, 5},
                 {4, 5}});
        });

    // Groups of similar test instances
    testInstances(
        "arborescence",
        {GraphProperty::arborescenceForest,
         GraphProperty::connected,
         GraphProperty::simple,
         GraphProperty::sparse},
        [](Graph& G, int n) { randomTree(G, n); });

    testInstances(
        "arborescence forest",
        {GraphProperty::arborescenceForest,
         GraphProperty::simple,
         GraphProperty::sparse},
        [](Graph& G, int n) {
            randomTree(G, n);

            // make graph disconnected
            for (int i = 0; i < std::min(3, G.numberOfEdges()); i++) {
                G.delEdge(G.chooseEdge());
            }
        });

    testInstances(
        "3-regular arborescence",
        {GraphProperty::arborescenceForest,
         GraphProperty::connected,
         GraphProperty::maxDeg4,
         GraphProperty::simple},
        [](Graph& G, int n) { regularTree(G, n, 3); });

    testInstances(
        "isolated nodes",
        {GraphProperty::arborescenceForest,
         GraphProperty::maxDeg4,
         GraphProperty::simple},
        [](Graph& G, int n) { emptyGraph(G, n); });

    testInstances(
        "connected sparse graph",
        {GraphProperty::connected,
         GraphProperty::simple,
         GraphProperty::sparse},
        [](Graph& G, int n) {
            randomSimpleGraph(G, n, 2 * n);
            makeConnected(G);
        });

    testInstances(
        "connected dense graph",
        {GraphProperty::connected, GraphProperty::simple},
        [](Graph& G, int n) {
            randomSimpleGraph(G, n, (n * n) / 4);
            makeConnected(G);
        });

    testInstances(
        "4-regular graph", {GraphProperty::maxDeg4}, [](Graph& G, int n) {
            randomRegularGraph(G, n, 4);
        });

    testInstances(
        "acyclic grid graph",
        {GraphProperty::acyclic,
         GraphProperty::biconnected,
         GraphProperty::maxDeg4,
         GraphProperty::planar,
         GraphProperty::simple},
        [](Graph& G, int n) {
            gridGraph(G, sqrt(n), sqrt(n), false, false);
        });

    testInstances(
        "wheel graph",
        {GraphProperty::biconnected,
         GraphProperty::planar,
         GraphProperty::simple},
        [](Graph& G, int n) { wheelGraph(G, n); });

    testInstances(
        "series parallel DAG",
        {GraphProperty::acyclic,
         GraphProperty::connected,
         GraphProperty::planar,
         GraphProperty::simple},
        [](Graph& G, int n) { randomSeriesParallelDAG(G, n); });

    testInstances(
        "path with multi-edges",
        {GraphProperty::connected,
         GraphProperty::loopFree,
         GraphProperty::planar},
        [](Graph& G, int n) {
            randomTree(G, n, 2, 1);
            addMultiEdges(G, .3);
            makeLoopFree(G);
        });

    testInstances(
        "connected planar graph",
        {GraphProperty::connected,
         GraphProperty::planar,
         GraphProperty::simple},
        [](Graph& G, int n) { randomPlanarConnectedGraph(G, n, 2 * n); });

    testInstances(
        "biconnected almost planar graph",
        {GraphProperty::biconnected,
         GraphProperty::nonPlanar,
         GraphProperty::simple,
         GraphProperty::sparse},
        [](Graph& G, int n) { createAlmostPlanarGraph(G, n, 2 * n, 10); });

    testInstances(
        "biconnected graph",
        {GraphProperty::biconnected,
         GraphProperty::simple,
         GraphProperty::sparse},
        [](Graph& G, int n) {
            randomBiconnectedGraph(G, n, 2 * n);
            splitParallelEdges(G);
        });

    testInstances(
        "acyclic biconnected planar graph",
        {GraphProperty::biconnected,
         GraphProperty::planar,
         GraphProperty::simple},
        [](Graph& G, int n) {
            randomPlanarBiconnectedDigraph(G, n, 2 * n);
            splitParallelEdges(G);
        });

    testInstances(
        "acyclic biconnected non-planar graph",
        {GraphProperty::biconnected,
         GraphProperty::nonPlanar,
         GraphProperty::simple,
         GraphProperty::sparse},
        [](Graph& G, int n) {
            randomBiconnectedGraph(G, n, 3 * n - 5);
            splitParallelEdges(G);
        });

    testInstances(
        "triconnected graph",
        {GraphProperty::simple, GraphProperty::triconnected},
        [](Graph& G, int n) { randomTriconnectedGraph(G, n, .5, .5); });

    testInstances(
        "triconnected planar graph",
        {GraphProperty::planar,
         GraphProperty::simple,
         GraphProperty::triconnected},
        [](Graph& G, int n) {
            randomPlanarTriconnectedGraph(G, n, .5, .5);
        });

    testInstances(
        "maximal planar graph",
        {GraphProperty::planar,
         GraphProperty::simple,
         GraphProperty::triconnected},
        [](Graph& G, int n) {
            randomPlanarBiconnectedGraph(G, n, 3 * n - 6);
        });

    testInstances(
        "disconnected planar graph",
        {GraphProperty::planar, GraphProperty::simple},
        [](Graph& G, int n) {
            createDisconnectedGraph(G, n, 1.4, 2.6, 3, 3);
        });

    testInstances(
        "planar dense triconnected multi-graph",
        {GraphProperty::planar, GraphProperty::triconnected},
        [](Graph& G, int n) {
            randomPlanarTriconnectedGraph(G, n, .5, .5);
            addMultiEdges(G, .5);
        });

    testInstances(
        "planar sparse triconnected multi-graph",
        {GraphProperty::planar,
         GraphProperty::sparse,
         GraphProperty::triconnected},
        [](Graph& G, int n) {
            randomPlanarTriconnectedGraph(G, n, .5, .5);
            addMultiEdges(G, std::min(5.0 / n, 0.95));
        });
}


inline void forEachGraphDescribe(
    std::set<GraphProperty>                    requirements,
    std::function<void(
        Graph&,
        const std::string&             graphName,
        const std::set<GraphProperty>& props)> doTest,
    GraphSizes                                 sizes   = GraphSizes(),
    int                                        minSize = 0,
    int maxSize = std::numeric_limits<int>::max()) {
    forEachGraphItWorks(
        requirements, doTest, sizes, minSize, maxSize, true);
}

inline void describeLayout(
    const std::string       name,
    LayoutModule&           L,
    long                    extraAttributes = 0,
    std::set<GraphProperty> req             = {},
    bool                    planarizes      = false,
    const GraphSizes&       sizes           = GraphSizes(),
    bool                    skipMe          = false) {
    forEachGraphItWorks(
        req,
        [&](const Graph&                   G,
            const std::string&             graphName,
            const std::set<GraphProperty>& props) {
            callLayout(
                graphName,
                G,
                L,
                extraAttributes,
                planarizes,
                doesInclude({GraphProperty::planar}, req),
                doesInclude({GraphProperty::planar}, props));
        },
        sizes);
}

template <typename T>
inline void describeLayout(
    const string&           name,
    int                     extraAttr  = 0,
    std::set<GraphProperty> req        = {},
    bool                    planarizes = false,
    const GraphSizes&       sizes      = GraphSizes(),
    bool                    skipMe     = false) {
    T layout;
    describeLayout(
        name, layout, extraAttr, req, planarizes, sizes, skipMe);
}

int main() {
    describeLayout<CPLMock>(
        "ClusterPlanarizationLayout",
        0,
        {GraphProperty::connected,
         GraphProperty::sparse,
         GraphProperty::simple},
        true,
        GraphSizes(16, 32, 16));
    std::cout << "Done execution\n";
}
