#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"

#include "llvm/Support/CommandLine.h"

#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

#include <iostream>

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;

class LoopPrinter : public MatchFinder::MatchCallback
{
  public:
    virtual void run(const MatchFinder::MatchResult& Result) {
        if (const ForStmt* FS = Result.Nodes.getNodeAs<clang::ForStmt>(
                "forLoop")) {
            FS->dumpColor();
        }
    }
};


// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory MyToolCategory("my-tool options");

// CommonOptionsParser declares HelpMessage with a description of the
// common command-line options related to the compilation database and
// input files. It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static cl::extrahelp MoreHelp("\nMore help text...\n");

int main(int argc, const char** argv) {
    std::cout << "starting clang main\n";
    auto ExpectedParser = CommonOptionsParser::create(
        argc, argv, MyToolCategory);
    if (!ExpectedParser) {
        // Fail gracefully for unsupported options.
        llvm::errs() << ExpectedParser.takeError();
        return 1;
    }
    CommonOptionsParser& OptionsParser = ExpectedParser.get();
    ClangTool            Tool(
        OptionsParser.getCompilations(),
        OptionsParser.getSourcePathList());

    LoopPrinter Printer;
    MatchFinder Finder;
    Finder.addMatcher(
        forStmt(hasLoopInit(declStmt(hasSingleDecl(
                    varDecl(hasInitializer(integerLiteral(equals(0))))))))
            .bind("forLoop"),
        &Printer);

    auto result = Tool.run(newFrontendActionFactory(&Finder).get());
    std::cout << "done clang main\n";
    return result;
}
