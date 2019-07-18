#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphml.hpp>
#include <iostream>
#include <string>

using namespace boost;

using Str = std::string;

struct ActualVertexProp {
    Str name;
};

struct VertexProp {
    ActualVertexProp prop;
};

inline static std::ostream& operator<<(
    std::ostream&           os,
    ActualVertexProp const& vp) {
    return os << std::quoted(vp.name);
}

inline static std::istream& operator>>(
    std::istream&     is,
    ActualVertexProp& vp) {
    return is >> std::quoted(vp.name);
}

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
        VertDesc vd_1 = add_vertex(graph);
        VertDesc vd_2 = add_vertex(graph);
        VertDesc vd_3 = add_vertex(graph);

        add_edge(vd_1, vd_2, "first_edge_property", graph);
        add_edge(vd_1, vd_3, "second_edge_property", graph);
        add_edge(vd_3, vd_2, "third_edge_property", graph);
    }

    dynamic_properties properties;
    properties.property("vertex_property", get(&VertexProp::prop, graph));
    properties.property("edge_property", get(&EdgeProp::name, graph));

    write_graphml(std::cout, graph, properties, true);
}
