#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Tooling.h>

#include <llvm/Support/CommandLine.h>

#include <clang/ASTMatchers/Dynamic/Parser.h>
#include <clang/ASTMatchers/ASTMatchers.h>
#include <clang/ASTMatchers/ASTMatchFinder.h>

#include <yaml-cpp/yaml.h>

#include <CLI/App.hpp>
#include <CLI/Formatter.hpp>
#include <CLI/Config.hpp>

#include <fmt/core.h>
#include <fmt/ranges.h>

#include <unordered_map>
#include <filesystem>
#include <vector>
#include <string>
#include <algorithm>
#include <optional>

namespace stdf = std::filesystem;

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;
using namespace clang::ast_matchers::dynamic;

template <typename T>
using Vec = std::vector<T>;
using Str = std::string;

template <typename T>
using Opt = std::optional<T>;

/// User-provided customization for handing of different procedure kinds
struct UserWrapRule {
    Str pattern;
    enum class ErrorHandling
    {
        NoErrors,   ///< Function does not produce errors during execution
        ReturnCode, ///< Function might return error code as a part of the
                    ///< result
    } errorHandling;

    Opt<Str> outArg; /// Mutable argument used as return value
};

/// Class to manage state required for the AST creation - `ASTContext`,
/// identifiers. Constructs AST nodes with no extra location information.
/// Class declares collection of constructor methods and structures - one
/// for each clang node kind. Structures allow using named parameters via
/// designated initializers (`.Cond = <expr>`)
class ASTBuilder
{
    ASTContext*     context;
    IdentifierTable idents;

    /// Create empty source location
    clang::SourceLocation sl() { return clang::SourceLocation(); }
    /// Get reference to stored context
    ASTContext& ctx() { return *context; }
    /// Get translation unit declaration
    DeclContext* dc() { return context->getTranslationUnitDecl(); }
    /// Create new identifier info from \arg name
    IdentifierInfo* id(const Str& name) { return &idents.get(name); }
    /// Create declaration namme from \arg name
    DeclarationName name(const Str& name) {
        return DeclarationName(id(name));
    }

  public:
    /// Update stored AST context
    void setContext(ASTContext* _context) { context = _context; }

    struct ParmVarDeclParams {
        QualType     type;
        Str          name;
        StorageClass storage = SC_None;
        Expr*        defArg  = nullptr;
        ParmVarDeclParams() {}
        ParmVarDeclParams(ParmVarDecl* decl)
            : type(decl->getOriginalType())
            , name(decl->getNameAsString())
            , storage(decl->getStorageClass())
            , defArg(decl->getDefaultArg()) {}
    };


    ParmVarDecl* ParmVarDecl(const ParmVarDeclParams& p) {
        return ParmVarDecl::Create(
            ctx(),
            dc(),
            sl(),
            sl(),
            id(p.name),
            p.type,
            nullptr,
            p.storage,
            p.defArg);
    }

    void setParams(FunctionDecl& decl, Vec<ParmVarDeclParams>& params) {
        Vec<class ParmVarDecl*> out;
        for (const auto& param : params) {
            out.push_back(ParmVarDecl(param));
        }

        decl.setParams(out);
    }


    struct FunctionDeclParams {
        Str           name;
        QualType      resultTy = QualType();
        Vec<QualType> argsTy   = {};
        StorageClass  storage  = SC_None;
    };

    FunctionDecl* FunctionDecl(
        FunctionDeclParams            p,
        const Vec<ParmVarDeclParams>& params = {}) {
        if (p.resultTy.isNull()) { p.resultTy = context->VoidTy; }
        //        if (!params.empty()) {
        //            p.argsTy.clear();
        //            for (const auto& param : params) {
        //                assert(!param.type.isNull());
        //                p.argsTy.push_back(param.type);
        //            }
        //        }

        return FunctionDecl::Create(
            ctx(),
            dc(),
            sl(),
            sl(),
            name(p.name),
            context->getFunctionType(
                p.resultTy, p.argsTy, FunctionProtoType::ExtProtoInfo()),
            nullptr,
            p.storage,
            false);
    }

    struct CompoundStmtParams {
        ArrayRef<Stmt*> Stmts;
    };

    CompoundStmt* CompoundStmt(const CompoundStmtParams& p) {
        return CompoundStmt::Create(ctx(), p.Stmts, sl(), sl());
    }

    struct VarDeclParams {
        Str          name;
        QualType     type;
        StorageClass storage = SC_None;
        Expr*        Init    = nullptr;
    };

    VarDecl* VarDecl(VarDeclParams p) {
        if (p.type.isNull()) { p.type = context->getAutoDeductType(); }

        auto result = VarDecl::Create(
            ctx(),
            dc(),
            sl(),
            sl(),
            id(p.name),
            p.type,
            nullptr,
            p.storage);

        if (p.Init != nullptr) { result->setInit(p.Init); }
        return result;
    }

    struct IfStmtParams {
        Expr*      Cond;
        Vec<Stmt*> Then;
        Vec<Stmt*> Else;

        Expr*           Init = nullptr;
        class VarDecl*  Var  = nullptr;
        IfStatementKind kind = IfStatementKind::Ordinary;
    };

    IfStmt* IfStmt(const IfStmtParams& p) {
        return IfStmt::Create(
            ctx(),
            sl(),
            p.kind,
            p.Init,
            p.Var,
            p.Cond,
            sl(),
            sl(),
            CompoundStmt({p.Then}),
            sl(),
            CompoundStmt({p.Else}));
    }

    class IfStmt* IfStmt(const ArrayRef<IfStmtParams>& p)
    {
        if (p.size() == 1) {
            return IfStmt(p[0]);
        } else {
            auto p0 = p[0];
            p0.Else = {IfStmt(p.slice(1))};
            return IfStmt(p0);
        }
    }

    DeclStmt* Stmt(Decl* decl) {
        return new (ctx()) DeclStmt(DeclGroupRef(decl), sl(), sl());
    }

    struct BinaryOperatorParams {
        BinaryOperator::Opcode opc;
        Expr*                  lhs;
        Expr*                  rhs;
        ExprValueKind          VK;
        ExprObjectKind         OK;
        FPOptionsOverride      FPFeatures;
    };

    BinaryOperator* BinaryOperator(const BinaryOperatorParams& p) {
        return BinaryOperator::Create(
            ctx(),
            p.lhs,
            p.rhs,
            p.opc,
            QualType(),
            p.VK,
            p.OK,
            sl(),
            p.FPFeatures);
    };


    DeclRefExpr* Ref(class VarDecl* decl) {
        return DeclRefExpr::Create(
            ctx(),
            NestedNameSpecifierLoc(),
            sl(),
            decl,
            false,
            sl(),
            decl->getType(),
            ExprValueKind::VK_LValue);
    }


    DeclRefExpr* Ref(const Str& name) {
        auto tmp = VarDecl({name, QualType()});
        return Ref(tmp);
    }

    DeclRefExpr* Ref(const class FunctionDecl* decl) {
        return Ref(decl->getName().str());
    }


    struct CallExprParams {
        Expr*      Fn;
        Vec<Expr*> Args = {};
    };

    CallExpr* CallExpr(const CallExprParams& p) {
        return CallExpr::Create(
            ctx(),
            p.Fn,
            p.Args,
            QualType(),
            ExprValueKind(),
            sl(),
            FPOptionsOverride());
    }

    IntegerLiteral* Literal(uint64_t value) {
        return IntegerLiteral::Create(
            ctx(), APInt(sizeof(value) * 8, value), ctx().IntTy, sl());
    }

    CXXThrowExpr* Throw(Expr* expr) {
        return new (ctx())
            CXXThrowExpr(expr, expr->getType(), sl(), false);
    }

    ReturnStmt* Return(Expr* expr) {
        return ReturnStmt::Create(ctx(), sl(), expr, nullptr);
    }
};

template <>
struct fmt::formatter<StringRef> : formatter<Str> {
    auto format(StringRef c, format_context& ctx) {
        return formatter<Str>::format(Str(c.data()), ctx);
    }
};

template <typename T>
Str clangToString(T* t) {
    Str                      out_str;
    llvm::raw_string_ostream outstream(out_str);
    t->print(outstream);
    return out_str;
}

Str clangToString(const QualType& t) {
    Str                      out_str;
    llvm::raw_string_ostream outstream(out_str);
    t.print(outstream, clang::PrintingPolicy(clang::LangOptions()));
    return out_str;
}

void operator>>(
    const YAML::Node&            node,
    UserWrapRule::ErrorHandling& error) {
    auto err = node.as<Str>();
    using E  = UserWrapRule::ErrorHandling;
    if (err == "none") {
        error = E::NoErrors;

    } else if (err == "code") {
        error = E::ReturnCode;
    } else {
    }
}

void operator>>(const YAML::Node& node, Str& str) { str = node.as<Str>(); }

void operator>>(const YAML::Node& node, UserWrapRule& rule) {
    // WARNING It looks like YAML parser documentation is outdated for
    // ~half a decade if not more (there is an issue dating back 2015) - it
    // says there is a `.FindValue` method, which is not the case. For
    // constant nodes `operator[]` returns 'null' node, for mutable it
    // might insert a new node. This API has the same pitfall as
    // `std::map::operator[]`, but short of iterating over all child nodes
    // I don't see any reasonable way to find a matching solution.
    if (node["function"]) {
        rule.pattern = fmt::format(
            R"(functionDecl(hasName("{}")))", node["function"].as<Str>());
    } else {
        node["patt"] >> rule.pattern;
    }

    if (node["out"]) { rule.outArg = node["out"].as<Str>(); }
    if (node["err"]) { node["err"] >> rule.errorHandling; }
}


/// Handle user-provided wrapper customization patterns - simply store them
/// in the list
struct CustomizerCollect : public MatchFinder::MatchCallback {
    UserWrapRule    rule;
    Vec<BoundNodes> bindings;
    inline CustomizerCollect(const UserWrapRule& _rule) : rule(_rule) {}
    virtual void run(const MatchFinder::MatchResult& result) override {
        llvm::outs() << "Customized collector rule matching triggered\n";
        bindings.push_back(result.Nodes);
    }
};


/// Execute conversion logic for each matching declaration. REFACTOR in the
/// future IR should be introduces and written to the output - codegen
/// could be handled by external tools.
class ConvertPusher : public MatchFinder::MatchCallback
{
    Vec<Str>               wrapped;   /// Wrapped node results
    Vec<CustomizerCollect> collector; /// Store results of the
                                      /// customized rules
    MatchFinder finder;               /// Finder for customizer rules
    ASTBuilder  b; /// Construct wrapped AST for processed
                   /// declarations

  public:
    /// Return matched user-provided rules that were triggered during last
    /// run of the finder.
    Vec<UserWrapRule> getMatchedRules() {
        Vec<UserWrapRule> result;
        for (const auto& it : collector) {
            if (0 < it.bindings.size()) { result.push_back(it.rule); }
        }
        return result;
    }

    void addUserRule(const UserWrapRule& rule) {
        Diagnostics Diag;
        StringRef   MatcherSource{rule.pattern};

        Optional<DynTypedMatcher> Matcher = Parser::parseMatcherExpression(
            MatcherSource, nullptr, nullptr, &Diag);

        collector.emplace_back(rule);
        finder.addDynamicMatcher(Matcher.getValue(), &collector.back());
    }

    /// Generate wrapping for the function \arg func using  provided \arg
    /// rule
    void wrapWithRule(const FunctionDecl* func, const UserWrapRule& rule) {
        llvm::outs() << "found triggered declaration " << func->getName()
                     << "\n";


        if (rule.outArg) {
            Str arguments;
            Str retType;
            Str argpass = "&out";

            QualType                           resultTy;
            Vec<ASTBuilder::ParmVarDeclParams> params;

            for (const auto& param : func->parameters()) {
                if (rule.outArg.value() == param->getName()) {
                    QualType    qtype = param->getOriginalType();
                    const Type* type  = qtype.getTypePtr();

                    if (type->isPointerType()) {
                        resultTy = type->getPointeeType();
                    }

                } else {
                    params.push_back(param);
                }
            }


            auto fd = b.FunctionDecl(
                {.name = func->getName().str(), .resultTy = resultTy},
                params);


            auto out = b.VarDecl({"out", resultTy});
            fd->setBody(b.CompoundStmt(
                {{b.Stmt(out),
                  b.Stmt(b.VarDecl(
                      {.name = "code",
                       .Init = b.CallExpr({b.Ref(func)})})),
                  b.IfStmt(
                      {.Cond = b.BinaryOperator(
                           {BinaryOperator::Opcode::BO_LT,
                            b.Ref(out),
                            b.Literal(0)}),
                       .Then = {b.Throw(b.Literal(0))},
                       .Else = {b.Return(b.Ref(out))}})}}));

            llvm::outs() << "-------------\n";
            fd->print(llvm::outs());
            llvm::outs() << "\n-------------\n";


            Str recall = fmt::format(
                R"(
{Out} {Name}({In}){{
    {Out} out;
    auto code = {Name}({Pass});
    if (code < 0) {{
        throw git::exception(code);
    }} else {{
        return out;
    }}
}})",
                fmt::arg("Out", retType),
                fmt::arg("Name", func->getName()),
                fmt::arg("In", arguments),
                fmt::arg("Pass", argpass));

            llvm::outs() << recall << "\n";
        }
    }

    virtual void run(const MatchFinder::MatchResult& Result) {
        auto func = Result.Nodes.getNodeAs<FunctionDecl>("function");
        b.setContext(Result.Context);

        for (auto& it : collector) {
            it.bindings.clear();
        }

        finder.match<Decl>(
            *Result.Nodes.getNodeAs<Decl>("function"),
            func->getASTContext());

        auto matched = getMatchedRules();

        if (0 < matched.size()) {
            wrapWithRule(func, matched[0]);
        } else {
        }
    }
};


int main(int argc, const char** argv) {
    cl::OptionCategory category("genwrapper");
    auto cli = CommonOptionsParser::create(argc, argv, category);

    if (!cli) {
        llvm::errs() << cli.takeError();
        return 1;
    }

    CommonOptionsParser& OptionsParser = cli.get();


    Vec<Str> files;
    for (const auto& in : OptionsParser.getSourcePathList()) {
        auto dir = stdf::path(in);
        if (stdf::is_regular_file(dir)) {
            files.push_back(in);
        } else {
            for (const auto& file : stdf::directory_iterator(dir)) {
                if (stdf::is_regular_file(file)) {
                    files.push_back(file.path().native());
                }
            }
        }
    }


    ClangTool Tool(OptionsParser.getCompilations(), files);

    ConvertPusher convert;
    MatchFinder   finder;

    { // Collect user-provided pattern matching rules
        YAML::Node doc = YAML::LoadFile("wrapconf.yaml");
        for (unsigned i = 0; i < doc.size(); i++) {
            UserWrapRule rule;
            doc[i] >> rule;
            convert.addUserRule(rule);
        }
    }

    finder.addMatcher(functionDecl().bind("function"), &convert);

    auto result = Tool.run(newFrontendActionFactory(&finder).get());

    std::cout << "ok\n";
}
