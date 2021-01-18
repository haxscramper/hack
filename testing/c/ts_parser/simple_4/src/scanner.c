#include <stdio.h>
#include <tree_sitter/parser.h>


enum Tok
{
    comment
};

void* tree_sitter_simple_4_external_scanner_create() {
    return NULL;
}


bool scan_comment(TSLexer* lexer) {
    lexer->mark_end(lexer);
    if (lexer->lookahead != '{') {
        return false;
    }

    lexer->advance(lexer, false);
    if (lexer->lookahead != '-') {
        return false;
    }

    lexer->advance(lexer, false);

    while (true) {
        switch (lexer->lookahead) {
            case '{': {
                scan_comment(lexer);
                break;
            }
            case '-': {
                lexer->advance(lexer, false);
                if (lexer->lookahead == '}') {
                    lexer->advance(lexer, false);
                    return true;
                }

                break;
            }
            case '\0': {
                return true;
            }
            default: {
                lexer->advance(lexer, false);
            }
        }
    }
}

bool tree_sitter_simple_4_external_scanner_scan(
    void*       payload,
    TSLexer*    lexer,
    const bool* valid_symbols) {


    if (valid_symbols[comment] && scan_comment(lexer)) {
        lexer->mark_end(lexer);
        lexer->result_symbol = comment;
        return true;
    }

    return false;
}

unsigned tree_sitter_simple_4_external_scanner_serialize(
    void* payload,
    char* buffer) {
    return 0;
}

void tree_sitter_simple_4_external_scanner_deserialize(
    void*       payload,
    const char* buffer,
    unsigned    length) {
}

void tree_sitter_simple_4_external_scanner_destroy(void* payload) {
}
