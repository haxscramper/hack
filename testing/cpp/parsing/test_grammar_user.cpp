#include "antlr_help_common.hpp"
#include "test_grammar_build/test_grammarLexer.h"
#include "test_grammar_build/test_grammarParser.h"

struct CustomLexer : Lexer {
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

    tokens.fill();

    test_grammarParser parser{&tokens};
    return executeParser(parser, lexer, &test_grammarParser::main);
}
