#include <stdio.h>
#include <tree_sitter/parser.h>


enum Tok
{
    NEWLINE,
    INDENT,
    DEDENT,
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

#define SKIP() lexer->advance(lexer, true);

#define AT(ch) (lexer->lookahead == ch)
#define SHOW printf("[\e[31m%c\e[39m]\n", lexer->lookahead);

typedef struct {
    int indent;
    int indent_stack[256];
    int indent_pos;
} scanner;

int current_indent(scanner* scan) {
    return scan->indent_stack[scan->indent_pos];
}

int pop_indent(scanner* scan) {
    int tmp = current_indent(scan);
    --scan->indent_pos;
    return tmp;
}

void push_indent(scanner* scan, int indent) {
    ++scan->indent_pos;
    scan->indent_stack[scan->indent_pos] = indent;
}

bool indent_stack_empty(scanner* scan) {
    return scan->indent_pos == 0;
}


bool tree_sitter_indentation_external_scanner_scan(
    void*       payload,
    TSLexer*    lexer,
    const bool* valid_symbols) {
    scanner* scan = (scanner*)(payload);

    bool found_line_end = false;
    int  indent_length  = 0;
    bool finished       = false;

    while (!finished) {
        switch (lexer->lookahead) {
            case '\n': {
                found_line_end = true;
                indent_length  = 0;
                SKIP();
                break;
            }
            case ' ': {
                ++indent_length;
                SKIP();
                break;
            }
            default: {
                finished = true;
                break;
            }
        }
    }

    if (found_line_end) {
        if (!indent_stack_empty(scan)) {
            int current_indent_length = current_indent(scan);
            if (valid_symbols[INDENT]
                && indent_length > current_indent_length) {
                push_indent(scan, indent_length);
                lexer->result_symbol = INDENT;
                return true;
            }

            if (valid_symbols[DEDENT]
                && indent_length < current_indent_length) {
                pop_indent(scan);
                lexer->result_symbol = DEDENT;
                return true;
            }
        }

        if (valid_symbols[NEWLINE]) {
            lexer->result_symbol = NEWLINE;
            return true;
        }
    }

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
