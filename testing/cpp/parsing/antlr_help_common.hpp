#include <iostream>
#include <strstream>
#include <string>

#include "antlr4-runtime.h"
#include <tree/TerminalNode.h>

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


std::string escapeLiteral(std::string const& in) {
    std::string res;
    res.reserve(in.size() + 2);
    res += "«";
    for (char c : in) {
        if (c == '\n') {
            res += "␤";

        } else {
            res += c;
        }
    }

    res += "»";

    return res;
}

void treeRepr(
    std::ostream&    os,
    const Parser&    parser,
    const Lexer&     lexer,
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
                treeRepr(
                    os, parser, lexer, tree->children.at(i), level + 1);
            }
            break;
        }
        case tree::ParseTreeType::TERMINAL: {
            auto terminal = dynamic_cast<tree::TerminalNode*>(tree);
            assert(terminal != nullptr);
            dfa::Vocabulary const& vocabulary = lexer.getVocabulary();
            Token*                 tok        = terminal->getSymbol();
            std::string_view       name       = vocabulary.getSymbolicName(
                tok->getType());
            if (name.size() == 0) {
                name = vocabulary.getLiteralName(tok->getType());
            }

            os << tok->getLine() << ":" << tok->getCharPositionInLine()
               << std::string(2 * level, ' ') << name << " '"
               << escapeLiteral(tree->getText()) << "'";
            break;
        }
        case tree::ParseTreeType::ERROR: {
            os << " ERROR";
            break;
        }
    }
}
