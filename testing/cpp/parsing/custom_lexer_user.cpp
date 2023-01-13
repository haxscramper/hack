#include <string_view>

#include "antlr_help_common.hpp"
#include "custom_lexer_build/custom_lexerLexer.h"
#include "custom_lexer_build/custom_lexerParser.h"

struct CustomLexer : Lexer {
    custom_lexerLexer* base;
    std::string_view   view;
    int                pos = 0;
    int                index;
    std::vector<int>   sliceStarts;

    CustomLexer(
        custom_lexerLexer* _base,
        const char*        data,
        std::size_t        size)
        : base(_base), view(data, size) {}

    template <typename K>
    struct ViewToken {
        K                kind;
        std::string_view view;
    };


    template <typename K>
    struct TToken
        : public Token
        , public ViewToken<K> {
        const char* base;
        size_t      index = 0;

        TToken(
            K                _kind,
            size_t           _index,
            const char*      _base,
            std::string_view _view)
            : ViewToken<K>{.kind = _kind, .view = _view}
            , base(_base)
            , index(_index) {}

        auto getView() const { return ViewToken<K>::view; };


        virtual size_t getType() const override {
            return ViewToken<K>::kind;
        }
        virtual std::string getText() const override {
            return std::string(getView().data(), getView().size());
        }
        virtual size_t getLine() const override {
            // TEMP placeholder for the implementation
            return 0;
        }
        virtual size_t getChannel() const override {
            // All tokens arriver in the same channel
            return 0;
        }
        virtual size_t getTokenIndex() const override { return index; }

        virtual size_t getStartIndex() const override {
            // start of the view-based token is an absolute distance
            // between underlying base string and view data.
            return std::abs(base - getView().data());
        }

        virtual size_t getStopIndex() const override {
            return getStartIndex() + getView().size();
        }
        virtual std::string toString() const override {
            return std::string(getView().data(), getView().size());
        }
        virtual CharStream* getInputStream() const override {
            return nullptr;
        }
        virtual TokenSource* getTokenSource() const override {
            return nullptr;
        }
        virtual size_t getCharPositionInLine() const override { return 0; }
    };

    enum OrgTokenKind
    {
        OTkBoldOpen  = custom_lexerParser::BoldOpen,
        OTkBoldClose = custom_lexerParser::BoldClose,
        OTkIdent     = custom_lexerParser::Ident,
        OTkSpace     = custom_lexerParser::Space,
        OTkEOF       = custom_lexerParser::EOF,
    };

    char get(int offset = 0) const { return view.at(pos + offset); }
    bool at(char c, int offset = 0) const { return get(offset) == c; }
    void next(int offset = 1) { pos += offset; }

    bool has(int offset) {
        return (0 <= offset + pos) && (offset + pos < view.size());
    }

    std::unique_ptr<TToken<OrgTokenKind>> token(OrgTokenKind kind) {
        int start = popSlice();

        auto res_view = std::string_view(view.data() + start, pos - start);
        const auto names = getRuleNames();
        return std::make_unique<TToken<OrgTokenKind>>(
            kind, index, view.data(), res_view);
        ++index;
    }

    int popSlice() {
        int result = sliceStarts.back();
        sliceStarts.pop_back();
        return result;
    }
    void pushSlice() { sliceStarts.push_back(pos); }

    std::unique_ptr<Token> nextToken() override {
        pushSlice();
        if (!has(0)) {
            return token(OTkEOF);
        }

        switch (get()) {
            case '*': {
                if (!has(-1) || get(-1) == ' ') {
                    next();
                    return token(OTkBoldOpen);
                } else {
                    next();
                    return token(OTkBoldClose);
                }
            }
            case ' ': {
                next();
                return token(OTkSpace);
            }
            case '\n': {
                next();
                return token(OTkSpace);
            }
            default: {
                while (('a' <= get() && get() <= 'z')
                       || ('A' <= get() && get() <= 'Z')) {
                    next();
                }
                return token(OTkIdent);
            }
        }
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
    std::ifstream file{argv[1]};
    file.seekg(0, std::ios::end);
    const int   size = file.tellg();
    std::string buffer(size, ' ');
    file.seekg(0);
    file.read(&buffer[0], size);

    ANTLRInputStream  input{file};
    custom_lexerLexer lexer{&input};

    CustomLexer       custom{&lexer, buffer.data(), buffer.size()};
    CommonTokenStream tokens{&custom};

    tokens.fill();

    custom_lexerParser parser{&tokens};
    executeParser(parser, &custom_lexerParser::main);
}
