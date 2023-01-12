#include <iostream>
#include <strstream>
#include <string>

#include "antlr4-runtime.h"

using namespace antlr4;

class MyParserErrorListener : public antlr4::BaseErrorListener {
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
    std::ostream&    os,
    const Parser&    parser,
    tree::ParseTree* tree,
    int              level) {

    tree::ParseTreeType type = tree->getTreeType();
    switch (type) {
        case tree::ParseTreeType::RULE: {
            ParserRuleContext* parserContext = dynamic_cast<
                ParserRuleContext*>(tree);

            auto   ruleIndex = parserContext->getRuleIndex();
            auto   name      = parser.getRuleNames()[ruleIndex];
            Token* start     = parserContext->getStart();
            Token* end       = parserContext->getStart();

            os <<                                     //
                start->getLine() << ":" <<            //
                start->getCharPositionInLine() <<     //
                std::string(2 * level, ' ') << " " << //
                name;

            for (int i = 0; i < tree->children.size(); ++i) {
                std::cout << "\n";
                treeRepr(os, parser, tree->children.at(i), level + 1);
            }
            break;
        }
        case tree::ParseTreeType::TERMINAL: {
            os << std::string(2 * level + 2, ' ') << " '"
               << tree->getText() << "'";
            break;
        }
        case tree::ParseTreeType::ERROR: {
            os << " ERROR";
            break;
        }
    }
}
