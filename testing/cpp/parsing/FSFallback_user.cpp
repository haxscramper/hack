#include "antlr_help_common.hpp"
#include "FSFallback_build/FSFallbackLexer.h"
#include "FSFallback_build/FSFallbackParser.h"

int main(int argc, char* argv[]) {
    return fullCycle<FSFallbackLexer, FSFallbackParser>(
        argc, argv, &FSFallbackParser::main);
}
