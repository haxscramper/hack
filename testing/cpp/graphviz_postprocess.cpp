#include <graphviz/gvc.h>
#include <graphviz/cgraph.h>
#include <cmath>
#include <sstream>
#include <string>
#include <stdio.h>
#include <iostream>

#include <string>
#include <sstream>
#include <regex>
#include <cmath>

std::string round_numbers(const std::string& input) {
    std::istringstream iss{input};
    std::stringstream  ss;
    std::string        s;
    bool               first = true;
    while (std::getline(iss, s, ' ')) {
        if (!first) {
            ss << " ";
        }
        first = false;

        std::regex re(R"((\d+(\.\d+)?))"); // Regular expression for float
                                           // numbers
        std::sregex_iterator it(s.begin(), s.end(), re);
        std::sregex_iterator reg_end;

        bool first = true;
        for (; it != reg_end; ++it) {
            double num         = std::stod(it->str());
            int    rounded_num = static_cast<int>(std::round(num / 10.0))
                            * 10; // Round to nearest 10

            if (!first) {
                ss << ",";
            }
            first = false;
            ss << rounded_num;
        }
    }

    return ss.str();
}

// Function to round a double to the nearest integer
double round_to_nearest(double num) { return round(num / 10) * 10; }

// Function to round the positions of all nodes and edges in a graph
void round_graph_positions(Agraph_t* graph) {
    auto fix = [](auto& val, float value = 5) {
        if (val < value) {
            val = value;
        } else {
            val = std::round((float)val / value) * value;
        }
    };

    auto print_node = [&](Agnode_t* node) {
        std::cout << "["
                  << "h:" << ND_height(node) << " w:" << ND_width(node)
                  << " x:" << ND_coord(node).x << " y:" << ND_coord(node).y
                  << "]";
        return "";
    };

    auto print_edge = [&](Agedge_t* edge) {
        std::cout << "[";
        for (int i = 0; i < ED_spl(edge)->list->size; i++) {
            auto& sp = ED_spl(edge)->list->list[i];
            if (i != 0) {
                std::cout << " ";
            }
            std::cout << sp.x << "," << sp.y;
        }
        std::cout << "]";
        return "";
    };

    for (Agnode_t* node = agfstnode(graph); node;
         node           = agnxtnode(graph, node)) {
        std::cout << "Node " << print_node(node);
        // Round node positioning and sizes
        fix(ND_ht(node));
        fix(ND_lw(node));
        //        fix(ND_width(node), 1);
        //        fix(ND_height(node), 1);

        fix(ND_coord(node).x);
        fix(ND_coord(node).y);
        std::cout << " --> " << print_node(node) << std::endl;
        std::vector<Agedge_t*> out;
        for (Agedge_t* edge = agfstout(graph, node); edge;
             edge           = agnxtout(graph, edge)) {
            out.push_back(edge);
        }

        if(out.empty()) {
            continue;
        }

        auto point = [](Agedge_t* edge, int idx) -> pointf& {
            return ED_spl(edge)->list->list[idx];
        };

        auto size = [](Agedge_t* edge) -> int {
           return ED_spl(edge)->list->size;
        };

        // After ortho layout finished, iterate over all outoing
        // edges and move their first 'link' to the same X-position
        // so they overlap on top of each other, instead of placing
        // multiple parallel links separately.
        for (auto& edge : out) {
            std::cout << "Edge " << print_edge(edge);
            // Iterate over all points in the edge and round the position

            for (int i = 0; i < size(edge); i++) {
                fix(point(edge, i).x);
                fix(point(edge, i).y);
            }

            std::cout << " --> " << print_edge(edge) << "\n";
        }

        auto target = point(out[0], 0).x;
        for(auto& edge: out) {
            auto start = point(edge, 0).x;
            for(int i = 0; i < size(edge); ++i) {
                if (point(edge, i).x == start) {
                    point(edge, i).x = target;
                }
            }
        }
    }
}

int main() {
    //    digraph G {
    //        splines=ortho;
    //        node[shape=rect];
    //        edge[arrowhead=none];
    //        a -> b;
    //        b -> c;
    //        c -> d;
    //        a -> d;
    //        a -> q;
    //        q -> d;
    //        a -> f;
    //    }

    GVC_t* gvc = gvContext();
    FILE*  fp  = fopen("/tmp/input.dot", "r");
    if (fp == NULL) {
        printf("Failed to open file\n");
        return 1;
    }

    Agraph_t* g = agread(fp, NULL);
    if (g == NULL) {
        printf("Failed to read graph\n");
        return 1;
    }

    gvLayout(gvc, g, "dot");

    gvRenderFilename(gvc, g, "xdot", "/tmp/unrounded_graph.xdot");
    gvRenderFilename(gvc, g, "png", "/tmp/unrounded_graph.png");

    round_graph_positions(g);

    gvRenderFilename(gvc, g, "xdot", "/tmp/rounded_graph.xdot");
    gvRenderFilename(gvc, g, "png", "/tmp/rounded_graph.png");

    gvFreeLayout(gvc, g);
    agclose(g);
    return (gvFreeContext(gvc));
}
