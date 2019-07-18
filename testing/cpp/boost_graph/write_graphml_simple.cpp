#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphml.hpp>
#include <iostream>
#include <string>

using namespace boost;

using Str = std::string;

struct VertexProp {
    VertexProp() = default;
    VertexProp(const char* tmp) : name(tmp){};
    Str name;
};

struct EdgeProp {
    EdgeProp() = default;
    EdgeProp(const char* tmp) : name(tmp){};
    Str name;
};

using Graph = adjacency_list<vecS, vecS, directedS, VertexProp, EdgeProp>;
using GraphTraits = graph_traits<Graph>;

using VertDesc        = GraphTraits::vertex_descriptor;
using VertBundledType = vertex_bundle_type<Graph>::type;
using EdgeBundledType = edge_bundle_type<Graph>::type;

int main() {
    Graph graph;

    { // Describe graph
        VertDesc vd_1 = add_vertex("first_vertex_property", graph);
        VertDesc vd_2 = add_vertex("second_vertex_property", graph);
        VertDesc vd_3 = add_vertex("third_vertex_property", graph);

        add_edge(vd_1, vd_2, "first_edge_property", graph);
        add_edge(vd_1, vd_3, "second_edge_property", graph);
        add_edge(vd_3, vd_2, "third_edge_property", graph);
    }

    dynamic_properties properties;
    properties.property("vertex_property", get(&VertexProp::name, graph));
    properties.property("edge_property", get(&EdgeProp::name, graph));

    write_graphml(std::cout, graph, properties, true);
}
