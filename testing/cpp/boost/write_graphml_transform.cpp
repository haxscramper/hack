#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphml.hpp>
#include <iostream>
#include <string>

// TODO write serialization using transform value property map
// https://stackoverflow.com/questions/49156829/write-read-vector-of-item-in-xml-using-boostwrite-graphml

using namespace boost;

using Str = std::string;

struct VertexProp {
    VertexProp() = default;
    VertexProp(const char* tmp) : name(tmp){};
    Str name;
};


inline static std::ostream& operator<<(
    std::ostream&     os,
    VertexProp const& vp) {
    return os << vp.name;
}

using Graph       = adjacency_list<vecS, vecS, directedS, VertexProp>;
using GraphTraits = graph_traits<Graph>;

int main() {
    Graph graph;

    add_vertex("first_vertex_property", graph);
    add_vertex("second_vertex_property", graph);
    add_vertex("third_vertex_property", graph);

    dynamic_properties properties;
    properties.property("vertex_property", get(&VertexProp::name, graph));

    write_graphml(std::cout, graph, properties, true);
}
