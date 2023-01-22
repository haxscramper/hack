#include <graphviz/gvc.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream>

int main(void) {
    // set up a graphviz context - but only once even for multiple graphs
    GVC_t* gvc = gvContext();

    // Create a simple digraph
    Agraph_t* g = agopen("g", Agdirected, 0);
    Agnode_t* n = agnode(g, "n", 1);
    Agnode_t* m = agnode(g, "m", 1);
    Agedge_t* e = agedge(g, n, m, 0, 1);

    // Set an attribute - in this case one that affects the visible
    // rendering
    agsafeset(n, "color", "red", "");

    // Use the directed graph layout engine
    gvLayout(gvc, g, "dot");
    // Output in .dot format
    FILE* out = fopen("/tmp/resfile.png", "w");
    gvRender(gvc, g, "png", out);
    fclose(out);
    gvFreeLayout(gvc, g);
    agclose(g);

    std::cout << "Wrote to out\n";
    return EXIT_SUCCESS;
}
