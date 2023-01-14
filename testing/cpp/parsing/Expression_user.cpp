#include "antlr_help_common.hpp"
#include "Expression_build/ExpressionLexer.h"
#include "Expression_build/ExpressionParser.h"

int main(int argc, char* argv[]) {
    return fullCycle<ExpressionLexer, ExpressionParser>(
        argc, argv, &ExpressionParser::main);
}
