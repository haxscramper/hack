#include <stdio.h>
#include <tree_sitter/parser.h>


enum Tok
{
    COMMENT
};

void* tree_sitter_simple_5_external_scanner_create() {
    return NULL;
}

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

bool tree_sitter_simple_5_external_scanner_scan(
    void*       payload,
    TSLexer*    lexer,
    const bool* valid_symbols) {
    while (lexer->lookahead == ' ') {
        lexer->advance(lexer, true);
    }

    int nested = 0;

    if (valid_symbols[COMMENT] && lexer->lookahead == '#') {
        lexer->mark_end(lexer);
        NEXT;
        if (lexer->lookahead == '[') {
            ++nested;
#if DEBUG
            printf("open start [%d]\n", nested);
#endif

            while (nested > 0 && !AT('\0')) {
                NEXT;
                if (AT('#')) {
                    NEXT;
                    if (AT('[')) {
                        ++nested;
#if DEBUG
                        printf("open internal [%d]\n", nested);
#endif
                    }
                } else if (AT(']')) {
                    NEXT;
                    if (AT('#')) {
                        --nested;
#if DEBUG
                        printf("close [%d]\n", nested);
#endif
                    }
                }
            }

#if DEBUG
            printf("Finished comment [%c]\n", lexer->lookahead);
#endif
            lexer->mark_end(lexer);

        } else {
            while (lexer->lookahead != '\n') {
                NEXT;
            }
            NEXT;
            lexer->mark_end(lexer);
        }

        lexer->advance(lexer, false);
        lexer->mark_end(lexer);
        lexer->result_symbol = COMMENT;
        return true;
    }
    return false;
}

unsigned tree_sitter_simple_5_external_scanner_serialize(
    void* payload,
    char* buffer) {
    return 0;
}

void tree_sitter_simple_5_external_scanner_deserialize(
    void*       payload,
    const char* buffer,
    unsigned    length) {
}

void tree_sitter_simple_5_external_scanner_destroy(void* payload) {
}
