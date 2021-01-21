#include <stdio.h>
#include <tree_sitter/parser.h>


enum Tok
{
    comment
};

void* tree_sitter_simple_4_external_scanner_create() {
    return NULL;
}

bool tree_sitter_simple_4_external_scanner_scan(
    void*       payload,
    TSLexer*    lexer,
    const bool* valid_symbols) {
    printf("[%c]\n", (lexer->lookahead == '\n' ? 'n' : lexer->lookahead));

    if (valid_symbols[comment] && lexer->lookahead == '{') {
        while (lexer->lookahead != '}' && lexer->lookahead != '\0') {
            lexer->advance(lexer, false);
        }
        lexer->advance(lexer, false);
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
