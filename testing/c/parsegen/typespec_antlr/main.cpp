#include <iostream>

#include <antlr4-runtime.h>

#include "resdir/typespecBaseListener.h"
#include "resdir/typespecLexer.h"
#include "resdir/typespecParser.h"

using namespace antlr4;
using Tree = tree::ParseTree;

void treeRepr(Tree* tree, int level = 0) {
    if (tree == nullptr) {
        return;
    } else {
        std::cout << level << " " << tree->getText() << "\n";
        for (Tree* subnode : tree->children) {
            treeRepr(subnode, level + 1);
        }
    }
}

int main(int argc, const char* argv[]) {
    ANTLRInputStream  input("seq[int]");
    typespecLexer     lexer(&input);
    CommonTokenStream tokens(&lexer);
    typespecParser    parser(&tokens);

    Tree* tree = parser.main();
    treeRepr(tree);

    return 0;
}
