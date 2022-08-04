// Declares clang::SyntaxOnlyAction.
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Frontend/CompilerInstance.h"
// Declares llvm::cl::extrahelp.
#include "llvm/Support/CommandLine.h"

#include "clang/AST/RecursiveASTVisitor.h"

#include <iostream>

using namespace clang;
using namespace clang::tooling;
using namespace llvm;

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory MyToolCategory("my-tool options");

class FindNamedClassVisitor
    : public RecursiveASTVisitor<FindNamedClassVisitor>
{
  public:
    explicit FindNamedClassVisitor(ASTContext* Context)
        : Context(Context) {}

    bool VisitCXXRecordDecl(CXXRecordDecl* Declaration) {
        if (Declaration->getQualifiedNameAsString() == "n::m::C") {
            FullSourceLoc FullLocation = Context->getFullLoc(
                Declaration->getBeginLoc());
            if (FullLocation.isValid()) {
                llvm::outs()
                    << "Found declaration at "
                    << FullLocation.getSpellingLineNumber() << ":"
                    << FullLocation.getSpellingColumnNumber() << "\n";
            }
        }
        return true;
    }

  private:
    ASTContext* Context;
};

class FindNamedClassConsumer : public clang::ASTConsumer
{
  public:
    explicit FindNamedClassConsumer(ASTContext* Context)
        : Visitor(Context) {}

    virtual void HandleTranslationUnit(clang::ASTContext& Context) {
        Visitor.TraverseDecl(Context.getTranslationUnitDecl());
    }

  private:
    FindNamedClassVisitor Visitor;
};

class FindNamedClassAction : public clang::ASTFrontendAction
{
  public:
    virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
        clang::CompilerInstance& Compiler,
        llvm::StringRef          InFile) {
        return std::make_unique<FindNamedClassConsumer>(
            &Compiler.getASTContext());
    }
};

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

    auto result = Tool.run(
        newFrontendActionFactory<FindNamedClassAction>().get());
    std::cout << "done clang main\n";
    return result;
}
