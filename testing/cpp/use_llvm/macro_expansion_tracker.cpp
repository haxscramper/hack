#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/CompilerInstance.h"

#include <fstream>

using namespace clang;

class MacroExpansionVisitor
    : public clang::RecursiveASTVisitor<MacroExpansionVisitor> {
  public:
    explicit MacroExpansionVisitor(
        clang::CompilerInstance& Instance,
        std::ofstream&           OutputFile)
        : SourceMgr(Instance.getSourceManager()), OutputFile(OutputFile) {}

    bool VisitStmt(clang::Stmt* S) {
        if (S->getBeginLoc().isMacroID()) {
            auto ExpansionRange = SourceMgr.getImmediateExpansionRange(
                S->getBeginLoc());
            auto StartLine = SourceMgr.getSpellingLineNumber(
                ExpansionRange.first);
            auto EndLine = SourceMgr.getSpellingLineNumber(
                ExpansionRange.second);
            OutputFile << "Macro expansion at lines " << StartLine
                       << " to " << EndLine << "\n";
        }

        return true;
    }

  private:
    clang::SourceManager& SourceMgr;
    std::ofstream&        OutputFile;
};

class MacroExpansionAction : public clang::ASTFrontendAction {
  public:
    std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
        clang::CompilerInstance& CI,
        llvm::StringRef) override {
        OutputFile.open("macro_expansions.txt");
        return std::make_unique<ASTConsumer>(
            *CI.getASTContext().getPrintingPolicy());
    }

  private:
    std::ofstream OutputFile;
};

#include "clang/Frontend/FrontendPluginRegistry.h"

static FrontendPluginRegistry::Add<MacroExpansionAction> X(
    "macro-expansion-tracker",
    "Trace macro expansions");
