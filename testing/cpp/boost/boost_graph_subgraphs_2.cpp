// There is a graph G0 and subgraphs G1...G7, and I want to find the
// occurrence of G7 in the set of subgraphs G1...G7, in this particular
// case the output should be    G7 is in 5, G7 is in 7 but it doesnt work
// and output is G7 is in 4, G7 is in 6, G7 is in 7. I don t really
// understand where the mistake could be.(maybe for-loop)
#include <boost/config.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graph_utility.hpp>
#include <boost/graph/graphviz.hpp>
#include <boost/graph/graph_utility.hpp>
#include <boost/graph/lookup_edge.hpp>
#include <boost/graph/subgraph.hpp>
#include <fstream>
#include <iostream>

using namespace boost;

typedef subgraph<adjacency_list<
    vecS,
    vecS,
    directedS,
    property<vertex_color_t, int>,
    property<edge_index_t, int>>>
    Graph;

int constexpr threshold        = 3;
int constexpr size_of_database = 7;

template <typename G>
bool is_subgraph_present(G const& g, G const& other) {
    Graph::edge_iterator ei, ei_end;
    unsigned             nj = 0;
    for (tie(ei, ei_end) = edges(g); ei != ei_end; ++ei) {

        auto gs = g.local_to_global(source(*ei, g));
        auto gt = g.local_to_global(target(*ei, g));
        // std::cout << "global (" << gs << "," << gt << ")\n";

        auto ls = other.find_vertex(gs);
        auto lt = other.find_vertex(gt);

        if (ls.second && lt.second) {
            // std::cout << "global (" << gs << "," << gt << ") local (" <<
            // ls.first << "," << lt.first << ")\n";

            if (edge(ls.first, lt.first, other).second) {
                nj++;
            }
        }
    }
    return nj == num_edges(g);
}


int main() {
    const int N = 6;
    enum
    {
        A,
        B,
        C,
        D,
        E,
        F
    }; // for conveniently refering to vertices in G0

    Graph  G0(N);
    Graph& G1 = G0.create_subgraph();
    Graph& G2 = G0.create_subgraph();
    Graph& G3 = G0.create_subgraph();
    Graph& G4 = G0.create_subgraph();
    Graph& G5 = G0.create_subgraph();
    Graph& G6 = G0.create_subgraph();
    Graph& G7 = G0.create_subgraph();

    enum
    {
        A1,
        B1,
        C1
    }; // for conveniently refering to vertices in G1
    enum
    {
        A2,
        B2
    }; // for conveniently refering to vertices in G2
    enum
    {
        A3,
        B3
    };
    enum
    {
        A4,
        B4,
        C4
    };
    enum
    {
        A5,
        B5
    };
    enum
    {
        A6,
        B6,
        C6
    };
    enum
    {
        A7,
        B7
    };

    add_vertex(C, G1); // global vertex C becomes local A1 for G1
    add_vertex(E, G1); // global vertex E becomes local B1 for G1
    add_vertex(F, G1); // global vertex F becomes local C1 for G1

    add_vertex(A, G2); // global vertex A becomes local A1 for G2
    add_vertex(B, G2); // global vertex B becomes local B1 for G2

    add_vertex(B, G3); // ...-||-...
    add_vertex(C, G3);

    add_vertex(A, G4);
    add_vertex(B, G4);
    add_vertex(E, G4);

    add_vertex(F, G5);
    add_vertex(D, G5);

    add_vertex(B, G6);
    add_vertex(D, G6);
    add_vertex(E, G6);

    add_vertex(F, G7);
    add_vertex(D, G7);

    add_edge(A, B, G0);
    add_edge(B, C, G0);
    add_edge(B, D, G0);
    add_edge(E, B, G0);
    add_edge(E, F, G0);
    add_edge(F, D, G0);

    // add_edge(F, C, G1); // (A1,C1) is subgraph G1 local indices for
    // (C,F).

    Graph::children_iterator ci, ci_end;

    int g_n = 1;

    for (tie(ci, ci_end) = G0.children(); ci != ci_end; ++ci) {
        if (is_subgraph_present(G7, *ci)) {
            std::cout << "G7 is in G" << g_n << std::endl;
        }
        g_n++;
    }

    std::cout << "G0:" << std::endl;
    print_graph(G0, get(vertex_index, G0));
    print_edges2(G0, get(vertex_index, G0), get(edge_index, G0));
    std::cout << std::endl;

    int num = 1;
    for (boost::tie(ci, ci_end) = G0.children(); ci != ci_end; ++ci) {
        std::cout << "G" << num++ << ":" << std::endl;
        print_graph(*ci, get(vertex_index, *ci));
        print_edges2(*ci, get(vertex_index, *ci), get(edge_index, *ci));
        std::cout << std::endl;
    }

    std::ofstream ofs("out.dot");
    // write_graphviz(ofs, G0); // TODO
}
