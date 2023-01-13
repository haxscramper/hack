#include <iostream>
#include <strstream>
#include <string>

#include "antlr4-runtime.h"
#include <tree/TerminalNode.h>

using namespace antlr4;

class ErrorListener : public antlr4::BaseErrorListener {
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

            os << "N " <<                             //
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
                // for anonymous tokens use the literal value name
                name = vocabulary.getLiteralName(tok->getType());
            }

            os
                // token index
                << "T [" << tok->getTokenIndex()
                << "]"
                // line start position
                << tok->getLine()
                // column position on the line
                << ":" << tok->getCharPositionInLine()
                << ".."
                // end column (not exactly correct for tokens that span
                // multiple lines but I don't think I can track the ed
                // column of the token)
                << (tok->getCharPositionInLine()
                    + (tok->getStopIndex() - tok->getStartIndex()))
                // indentation
                << std::string(2 * level, ' ')
                // best-guess name
                << name
                << " "
                // cleaned up literal value
                << escapeLiteral(tree->getText());
            break;
        }
        case tree::ParseTreeType::ERROR: {
            os << "E ERROR";
            break;
        }
    }
}

template <typename Parse, typename ResultContext>
int executeParser(
    Parse& parser,
    Lexer& lexer,
    // Using separate generic parameter because start fuction always
    // returns its own type, derived from the ParserRuleContext (which in
    // turns is derived from the ParseTree)
    ResultContext* (Parse::*startFunction)()) {
    parser.removeErrorListeners();
    parser.addErrorListener(new ErrorListener());
    try {
        tree::ParseTree* tree = (parser.*startFunction)();
        std::cout << tree->toStringTree() << std::endl;
        treeRepr(std::cout, parser, lexer, tree, 0);
        std::cout << "\ndone" << std::endl;
        return 0;
    } catch (std::invalid_argument& e) {
        std::cout << e.what() << std::endl;
        return 10;
    }
}

template <typename Lexer, typename Parser, typename ResultContext>
int fullCycle(
    int    argc,
    char** argv,
    ResultContext* (Parser::*startFunction)()) {
    std::ifstream file{};
    file.open(argv[1]);
    ANTLRInputStream  input{file};
    Lexer             lexer{&input};
    CommonTokenStream tokens{&lexer};

    tokens.fill();

    Parser parser{&tokens};
    return executeParser(parser, lexer, startFunction);
}
