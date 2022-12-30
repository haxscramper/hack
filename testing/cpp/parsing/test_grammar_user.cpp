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
    std::ostream&             os,
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

struct CustomLexer : Lexer
{
    test_grammarLexer* base;
    CustomLexer(test_grammarLexer* _lexer) : base(_lexer) {}

    std::unique_ptr<Token> nextToken() override {
        auto result = base->nextToken();
        return result;
    }

    // Passthrough for the lexer API, these things might be genrated
    // automatically later on.
    std::vector<std::string> const& getRuleNames() const override {
        return base->getRuleNames();
    }
    dfa::Vocabulary const& getVocabulary() const override {
        return base->getVocabulary();
    }
    std::string getGrammarFileName() const override {
        return base->getGrammarFileName();
    }
    const atn::ATN& getATN() const override { return base->getATN(); }
    const std::vector<std::string>& getChannelNames() const override {
        return base->getChannelNames();
    }
    const std::vector<std::string>& getModeNames() const override {
        return base->getModeNames();
    }
};

int main(int argc, char* argv[]) {
    std::ifstream file{};
    file.open(argv[1]);
    ANTLRInputStream  input{file};
    test_grammarLexer lexer{&input};
    CustomLexer       custom{&lexer};
    CommonTokenStream tokens{&custom};

    MyParserErrorListener errorListner;

    tokens.fill();

    test_grammarParser parser{&tokens};
    parser.removeErrorListeners();
    parser.addErrorListener(&errorListner);
    try {
        tree::ParseTree* tree = parser.main();
        std::cout << tree->toStringTree() << std::endl;
        treeRepr(std::cout, parser, tree, 0);
        std::cout << "\ndone" << std::endl;
        return 0;
    } catch (std::invalid_argument& e) {
        std::cout << e.what() << std::endl;
        return 10;
    }
}
