#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graph_utility.hpp>
#include <boost/graph/subgraph.hpp>

#include <boost/graph/graphviz.hpp>

using namespace boost;

struct EdgeProp {};

// Define your graph type
typedef adjacency_list<
    vecS,
    vecS,
    undirectedS,
    property<vertex_index_t, int>,
    property<edge_index_t, int, property<edge_attribute_t, EdgeProp>>>
    Graph;

// Define your subgraph type
typedef subgraph<Graph> SubGraph;

int main() {
    // Create a global graph with 5 vertices
    SubGraph g;

    // Create a subgraph
    SubGraph subG = g.create_subgraph();

    // Add a vertex to the subgraph
    SubGraph::vertex_descriptor v = add_vertex(subG);

    // Get the global vertex descriptor corresponding to v
    Graph::vertex_descriptor global_v = get(vertex_index, subG, v);

    // Print the global vertex index
    std::cout << "Global vertex index: " << global_v << std::endl;

    using GlobalIndexProperty = global_property<vertex_index_t>;
    // Now, let's use subgraph_global_property_map
    using GlobalVertexIndexMap = property_map<
        SubGraph,
        GlobalIndexProperty>::type;

    GlobalVertexIndexMap globalIndexMap;

    // Access the global index of v using the global property map
    std::cout << "Global vertex index (via subgraph_global_property_map): "
              << get(globalIndexMap, v) << std::endl;


    // TODO fix compilation and get property from global map
#if false
    using GlobalAttributeMap = global_property<EdgeProp>;
    using GlobalAtributeMap  = property_map<SubGraph, GlobalAttributeMap>::
        type;

    GlobalAttributeMap globalAttributeMap = global(EdgeProp{});
#endif


    return 0;
}
