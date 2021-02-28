#include <stdio.h>
#include <tree_sitter/parser.h>


enum Tok
{
    COMMENT
};

#define DEBUG 0

#if DEBUG
#    define NEXT                                                          \
        printf("[\e[32m%c\e[39m] -> ", lexer->lookahead);                 \
        (lexer->advance(lexer, true));                                    \
        printf("[\e[32m%c\e[39m]\n", lexer->lookahead);
#else

#    define NEXT (lexer->advance(lexer, true));

#endif

#define AT(ch) (lexer->lookahead == ch)
#define SHOW printf("[\e[31m%c\e[39m]\n", lexer->lookahead);

typedef struct {
    int indent;
} scanner;

bool tree_sitter_indentation_external_scanner_scan(
    void*       payload,
    TSLexer*    lexer,
    const bool* valid_symbols) {
    scanner* scan = (scanner*)(payload);


    return false;
}

unsigned tree_sitter_indentation_external_scanner_serialize(
    void* payload,
    char* buffer) {
    return 0;
}

void tree_sitter_indentation_external_scanner_deserialize(
    void*       payload,
    const char* buffer,
    unsigned    length) {
}

void* tree_sitter_indentation_external_scanner_create() {
    return malloc(sizeof(scanner));
}


void tree_sitter_indentation_external_scanner_destroy(void* payload) {
    free(payload);
}
