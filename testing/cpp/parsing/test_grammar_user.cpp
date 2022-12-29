#include <iostream>
#include <strstream>
#include <string>

#include "antlr4-runtime.h"

#include "test_grammar_build/test_grammarLexer.h"
#include "test_grammar_build/test_grammarParser.h"

using namespace antlr4;

class MyParserErrorListener : public antlr4::BaseErrorListener
{
    virtual void syntaxError(
        Recognizer*        recognizer,
        Token*             offendingSymbol,
        size_t             line,
        size_t             charPositionInLine,
        const std::string& msg,
        std::exception_ptr e) override {
        std::ostrstream s;
        s << "Line(" << line << ":" << charPositionInLine << ") Error("
          << msg << ")";
        throw std::invalid_argument(s.str());
    }
};


void treeRepr(
    const test_grammarParser& parser,
    tree::ParseTree*          tree,
    int                       level) {

    tree::ParseTreeType type = tree->getTreeType();
    switch (type) {
        case tree::ParseTreeType::RULE: {
            ParserRuleContext* parserContext = dynamic_cast<
                ParserRuleContext*>(tree);

            auto   ruleIndex = parserContext->getRuleIndex();
            auto   name      = parser.getRuleNames()[ruleIndex];
            Token* start     = parserContext->getStart();
            Token* end       = parserContext->getStart();

            std::cout <<                              //
                start->getLine() << ":" <<            //
                start->getCharPositionInLine() <<     //
                std::string(2 * level, ' ') << " " << //
                name;

            for (int i = 0; i < tree->children.size(); ++i) {
                std::cout << "\n";
                treeRepr(parser, tree->children.at(i), level + 1);
            }
            break;
        }
        case tree::ParseTreeType::TERMINAL: {
            std::cout << std::string(2 * level + 2, ' ') << " '"
                      << tree->getText() << "'";
            break;
        }
        case tree::ParseTreeType::ERROR: {
            std::cout << " ERROR";
            break;
        }
    }
}

int main(int argc, char* argv[]) {
    std::ifstream file{};
    file.open(argv[1]);
    ANTLRInputStream  input{file};
    test_grammarLexer lexer{&input};
    CommonTokenStream tokens{&lexer};

    MyParserErrorListener errorListner;

    tokens.fill();

    test_grammarParser parser{&tokens};
    parser.removeErrorListeners();
    parser.addErrorListener(&errorListner);
    try {
        tree::ParseTree* tree = parser.main();
        std::cout << tree->toStringTree() << std::endl;
        treeRepr(parser, tree, 0);
        return 0;
    } catch (std::invalid_argument& e) {
        std::cout << e.what() << std::endl;
        return 10;
    }

    std::cout << "done" << std::endl;
}
