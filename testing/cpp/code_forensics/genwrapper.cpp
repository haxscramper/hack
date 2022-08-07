#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Tooling.h>

#include <llvm/Support/CommandLine.h>

#include <clang/ASTMatchers/Dynamic/Parser.h>
#include <clang/ASTMatchers/ASTMatchers.h>
#include <clang/ASTMatchers/ASTMatchFinder.h>

#include <yaml-cpp/yaml.h>

#include <fmt/core.h>
#include <fmt/ranges.h>

#include <unordered_map>
#include <filesystem>
#include <vector>
#include <string>
#include <algorithm>
#include <optional>
#include <iostream>
#include <fstream>
#include <exception>
#include <memory>

#include <range/v3/all.hpp>


#define BOOST_STACKTRACE_USE_ADDR2LINE
#define BOOST_STACKTRACE_ADDR2LINE_LOCATION "/bin/addr2line"

#include <boost/stacktrace.hpp>
#include <boost/exception/all.hpp>

#include <cxxabi.h>

using traced = boost::
    error_info<struct tag_stacktrace, boost::stacktrace::stacktrace>;

template <class E>
void throw_with_trace(const E& e) {
    throw boost::enable_error_info(e)
        << traced(boost::stacktrace::stacktrace(1, -1));
}

void boost_terminate_handler() {
    try {
        std::cerr << boost::stacktrace::stacktrace();
    } catch (...) {}
    std::abort();
}

inline std::string cxxDemangle(char const* mangled) {
    int   status;
    char* ret = abi::__cxa_demangle(mangled, nullptr, nullptr, &status);
    switch (status) {
        case 0: break;
        case -1: throw std::bad_alloc();
        case -2:
            throw std::logic_error(
                "Not a proper mangled name" + std::string(mangled));
        case -3:
            throw std::invalid_argument(
                "Invalid argument to __cxa_demangle");
        default:
            throw std::logic_error("Unknown error from __cxa_demangle");
    }
    std::string name(ret);
    free(ret);
    return name;
}

namespace stdf = std::filesystem;

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;
using namespace clang::ast_matchers::dynamic;

namespace rv = ranges::views;

template <typename T>
using Vec = std::vector<T>;
using Str = std::string;

template <typename T>
using Opt = std::optional<T>;

template <typename T>
using UPtr = std::unique_ptr<T>;

/// User-provided customization for handing of different procedure kinds
struct UserWrapRule {
    Str pattern;
    enum class ErrorHandling
    {
        NoErrors,   ///< Function does not produce errors during execution
        ReturnCode, ///< Function might return error code as a part of the
                    ///< result
    } errorHandling;

    bool     ignore = false;
    Opt<Str> outArg; /// Mutable argument used as return value
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


    /// Function declaration signature
    struct FunctionDeclParams {
        Str           Name;
        QualType      ResultTy = QualType();
        Vec<QualType> ArgsTy   = {};
        StorageClass  Storage  = SC_None;
        Stmt*         Body     = nullptr;
        bool          Inline   = false;
    };


    /// Create function declaration. If \arg params is not empty use it to
    /// construct argument types in the resulting function signature.
    FunctionDecl* FunctionDecl(
        FunctionDeclParams            p,
        const Vec<ParmVarDeclParams>& params = {}) {
        if (p.ResultTy.isNull()) { p.ResultTy = context->VoidTy; }
        Vec<class ParmVarDecl*> passParams;
        if (!params.empty()) {
            p.ArgsTy.clear();
            for (const auto& param : params) {
                passParams.push_back(ParmVarDecl(param));
                assert(!param.type.isNull());
                p.ArgsTy.push_back(param.type);
            }
        }

        auto res = FunctionDecl::Create(
            ctx(),
            dc(),
            sl(),
            sl(),
            name(p.Name),
            context->getFunctionType(
                p.ResultTy, p.ArgsTy, FunctionProtoType::ExtProtoInfo()),
            nullptr,
            p.Storage,
            p.Inline);
        res->setParams(passParams);
        if (p.Body != nullptr) { res->setBody(p.Body); }
        return res;
    }

    struct CompoundStmtParams {
        ArrayRef<Stmt*> Stmts;
    };

    CompoundStmt* CompoundStmt(const CompoundStmtParams& p) {
        return CompoundStmt::Create(ctx(), p.Stmts, sl(), sl());
    }

    struct VarDeclParams {
        Str          Name;
        QualType     Type;
        StorageClass Storage = SC_None;
        Expr*        Init    = nullptr;
    };

    VarDecl* VarDecl(VarDeclParams p) {
        if (p.Type.isNull()) { p.Type = context->getAutoDeductType(); }

        auto result = VarDecl::Create(
            ctx(),
            dc(),
            sl(),
            sl(),
            id(p.Name),
            p.Type,
            nullptr,
            p.Storage);

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
            p.Else.empty() ? nullptr : CompoundStmt({p.Else}));
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

    class BinaryOperator* BinaryOperator(const BinaryOperatorParams& p)
    {
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

    class BinaryOperator* XCall(
        const BinaryOperator::Opcode& opc,
        Vec<Expr*>                    args)
    {
        return BinaryOperator(
            {.opc = opc, .lhs = args[0], .rhs = args[1]});
    }

    struct UnaryOperatorParams {
        UnaryOperator::Opcode opc;
        Expr*                 Expr;
        ExprValueKind         VK;
        ExprObjectKind        OK;
        FPOptionsOverride     FPFeatures;
    };

    class UnaryOperator* UnaryOperator(const UnaryOperatorParams& p)
    {
        return clang::UnaryOperator::Create(
            ctx(),
            p.Expr,
            p.opc,
            ctx().VoidTy,
            p.VK,
            p.OK,
            sl(),
            false,
            p.FPFeatures);
    }

    class UnaryOperator* XCall(const UnaryOperator::Opcode opc, Expr* expr)
    {
        return UnaryOperator({.opc = opc, .Expr = expr});
    }


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

    class CallExpr* CallExpr(const CallExprParams& p)
    {
        return CallExpr::Create(
            ctx(),
            p.Fn,
            p.Args,
            QualType(),
            ExprValueKind(),
            sl(),
            FPOptionsOverride());
    }

    class CallExpr* XCall(const Str& name, Vec<Expr*> Args = {})
    {
        return CallExpr({Ref(name), Args});
    }

    class CallExpr* XCall(
        const class FunctionDecl* decl,
        Vec<Expr*>                Args = {})
    {
        return CallExpr({Ref(decl), Args});
    }

    IntegerLiteral* Literal(uint64_t value) {
        return IntegerLiteral::Create(
            ctx(), APInt(sizeof(value) * 8, value), ctx().IntTy, sl());
    }

    clang::StringLiteral* Literal(const Str& str) {
        return clang::StringLiteral::Create(
            ctx(),
            str,
            clang::StringLiteral::StringKind::Ascii,
            false,
            QualType(),
            sl());
    }

    CXXThrowExpr* Throw(Expr* expr) {
        return new (ctx())
            CXXThrowExpr(expr, expr->getType(), sl(), false);
    }

    ReturnStmt* Return(Expr* expr) {
        return ReturnStmt::Create(ctx(), sl(), expr, nullptr);
    }

    class Expr* Expr(Expr* expr)
    {
        return expr;
    }
};

template <>
struct fmt::formatter<StringRef> : formatter<Str> {
    auto format(StringRef c, format_context& ctx) {
        return formatter<Str>::format(Str(c.data()), ctx);
    }
};

struct yaml_processing_error : std::exception {
    const Str msg;
    yaml_processing_error(const Str& _in) : msg(_in) {}
    virtual const char* what() const noexcept override {
        return msg.c_str();
    }
};

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
        throw_with_trace(yaml_processing_error(
            fmt::format("Unknown error handling strategy {}", err)));
    }
}


void operator>>(const YAML::Node& node, Str& str) { str = node.as<Str>(); }
void operator>>(const YAML::Node& node, bool& str) {
    str = node.as<bool>();
}

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
    } else if (node["patt"]) {
        node["patt"] >> rule.pattern;
    } else {
        throw_with_trace(yaml_processing_error(
            "Either 'function' or 'patt' should be used"));
    }

    if (node["out"]) { rule.outArg = node["out"].as<Str>(); }
    if (node["err"]) { node["err"] >> rule.errorHandling; }
    if (node["ignore"]) { node["ignore"] >> rule.ignore; }
}


/// Handle user-provided wrapper customization patterns - simply store them
/// in the list
struct CustomizerCollect : public MatchFinder::MatchCallback {
    UserWrapRule    rule;
    Vec<BoundNodes> bindings;
    inline CustomizerCollect(const UserWrapRule& _rule) : rule(_rule) {}
    virtual void run(const MatchFinder::MatchResult& result) override {
        bindings.push_back(result.Nodes);
    }
};


struct SubFinder : public MatchFinder {
    UPtr<CustomizerCollect> collector;
    SubFinder(const UserWrapRule& rule)
        : collector(new CustomizerCollect(rule)) {
        Diagnostics Diag;
        StringRef   MatcherSource{rule.pattern};

        Optional<DynTypedMatcher> Matcher = Parser::parseMatcherExpression(
            MatcherSource, nullptr, nullptr, &Diag);

        assert(Matcher.hasValue());
        addDynamicMatcher(Matcher.getValue(), collector.get());
    }
};

/// Execute conversion logic for each matching declaration. REFACTOR in the
/// future IR should be introduces and written to the output - codegen
/// could be handled by external tools.
class ConvertPusher : public MatchFinder::MatchCallback
{
    Vec<Str>       wrapped; /// Wrapped node results
    Vec<SubFinder> finder;  /// Finder for customizer rules. Patterns are
                            /// tried in order, from the first to the last.
                            /// First triggered rule is used - approach
                            /// allows prioritization of the more specific
    /// rules over generic `functionDecl()` pattern.
    ASTBuilder b; /// Construct wrapped AST for processed
                  /// declarations

    std::function<Str(const Str&)> renameCb; /// Change declaration name
    std::function<bool(const DynTypedNode&)> allowCb; /// Allow or skip
                                                      /// declaration

  public:
    const Vec<Str>& getWrapped() { return wrapped; }
    void            setRenameCb(std::function<Str(const Str&)> _rename) {
                   renameCb = _rename;
    }

    void setAllowCb(std::function<bool(const DynTypedNode&)> impl) {
        allowCb = impl;
    }


    void addUserRule(const UserWrapRule& rule) { finder.push_back(rule); }

    /// Generate wrapping for the function \arg func using  provided \arg
    /// rule
    void wrapWithRule(const FunctionDecl* func, const UserWrapRule& rule) {
        Vec<ASTBuilder::ParmVarDeclParams> Params;

        // Declare function builder parameters and start pupulating it's
        // parts as input is processed
        ASTBuilder::FunctionDeclParams DeclParams{
            // Default to `void` return type
            .ResultTy = func->getASTContext().VoidTy,
            .Inline   = true};


        // Function body is constructed along the way
        Vec<Stmt*> Body;
        // Expressions for passing arguments to the original function
        Vec<Expr*> Pass;

        // If 'out' argument is present it must be found and handled
        // differently
        if (rule.outArg) {
            for (const auto& param : func->parameters()) {
                if (rule.outArg.value() == param->getName()) {
                    QualType    qtype = param->getOriginalType();
                    const Type* type  = qtype.getTypePtr();

                    if (type->isPointerType()) {
                        // Assuming double pointer (`T**`) for now, in the
                        // future this might change
                        DeclParams.ResultTy = type->getPointeeType();
                        Body.push_back(b.Stmt(b.VarDecl(
                            {rule.outArg.value(), DeclParams.ResultTy})));

                        Pass.push_back(b.XCall(
                            UnaryOperator::Opcode::UO_AddrOf,
                            b.Ref(param)));
                    }

                } else {
                    // This is not an 'out' argument, process it in a
                    // regular manner (passthrough)
                    Params.push_back(param);
                    Pass.push_back(b.Ref(param));
                }
            }


        } else {
            // No 'out' argument, process everything in one go
            for (const auto& param : func->parameters()) {
                Params.push_back(param);
                Pass.push_back(b.Ref(param));
            }
        }


        // Construct function call AST
        auto call      = b.XCall(func, Pass);
        bool hasResult = false;


        if (func->getCallResultType()->isVoidType()) {
            // If no result is expected simply append call to the body
            Body.push_back(call);

        } else {
            // Otherwise assign it to the `code` output
            hasResult = true;
            Body.push_back(
                b.Stmt(b.VarDecl({.Name = "__result", .Init = call})));
        }

        // Failure code is user-customizable - generator only adds call to
        // the macro and passes the relevant information to it.
        auto Fail = b.XCall(
            "__GIT_THROW_EXCEPTION",
            {b.Ref("__result"), b.Literal(func->getName().str())});

        switch (rule.errorHandling) {
            case UserWrapRule::ErrorHandling::NoErrors: {
                // If error should not be handled for this function, use
                // it's output value as a final result
                if (hasResult) {
                    Body.push_back(b.Return(b.Ref("__result")));
                    DeclParams.ResultTy = func->getCallResultType();
                }
                //                outs() << "No errors for " <<
                //                func->getName() << "\n";
                break;
            }
            case UserWrapRule::ErrorHandling::ReturnCode: {
                // Otherwise process output if there is an explicit result
                // in the function call.
                if (hasResult) {
                    ASTBuilder::IfStmtParams IfParams{
                        .Cond = b.XCall(
                            BinaryOperator::Opcode::BO_LT,
                            {b.Ref("__result"), b.Literal(0)}),
                        // If code is less than zero (HACK, different
                        // libraries might have other strategy for error
                        // denotation) handle it as error
                        .Then = {Fail}};

                    if (rule.outArg) {
                        // If out argument is present return it in the
                        // 'else' branch
                        IfParams.Else = {
                            b.Return(b.Ref(rule.outArg.value()))};
                    } /* else if (!func->getCallResultType()->isVoidType())
                     { IfParams.Else = {b.Return(b.Ref("__result"))};
                     }*/
                    Body.push_back(b.IfStmt(IfParams));
                }
                break;
            }
        }

        // Create compound statement from all collected statements
        DeclParams.Body = b.CompoundStmt({Body});
        // If rename callback is present use it, otherwise assign the same
        // name as before
        DeclParams.Name =
            (renameCb ? renameCb(func->getName().str())
                      : func->getName().str());

        // Convert wrapped function to string and push it to the output
        // list
        wrapped.push_back(
            clangToString(b.FunctionDecl(DeclParams, Params)));
    }

    /// Override of the match finder result handing.
    virtual void run(const MatchFinder::MatchResult& Result) {
        auto func = Result.Nodes1.getNodeAs<FunctionDecl>("function");
        if (allowCb &&
            !allowCb(DynTypedNode::create<FunctionDecl>(*func))) {
            return;
        }

        b.setContext(Result.Context);

        for (auto& find : finder) {
            find.match<FunctionDecl>(*func, func->getASTContext());
            if (0 < find.collector->bindings.size()) {
                if (!find.collector->rule.ignore) {
                    wrapWithRule(func, find.collector->rule);
                }
                find.collector->bindings.clear();
                break;
            }
        }
    }
};


int main_impl(int argc, const char** argv) {
    std::set_terminate(&boost_terminate_handler);

    cl::OptionCategory category("genwrapper");
    // Optional output filename to write converted libraries to
    cl::opt<Str> OutputFilename(
        "o",
        cl::desc("Specify output filename"),
        cl::value_desc("filename"),
        cl::cat(category));


    auto cli = CommonOptionsParser::create(argc, argv, category);

    if (!cli) {
        llvm::errs() << cli.takeError();
        return 1;
    }

    CommonOptionsParser& OptionsParser = cli.get();


    // Expand any directory names used in the parameter list
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

    // HACK - remove first four letters in the function names, because I'm
    // working with `git_` library right now. In future should be
    // refactored to a more general rename tool
    convert.setRenameCb([](const Str& in) { return in.substr(4); });
    convert.setAllowCb([](const DynTypedNode& node) {
        auto      func = node.get<FunctionDecl>();
        StringRef path = func->getASTContext()
                             .getSourceManager()
                             .getFilename(func->getLocation());
        if (path.contains("deprecated.h")) { return false; }
        auto name = func->getNameAsString();

        auto result = name.rfind("git_", 0) == 0;
        return result;
    });

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

    if (!OutputFilename.empty()) {
        Str           name = OutputFilename.getValue();
        std::ofstream out{name};
        out << "#pragma once\n";
        for (const auto& entry : convert.getWrapped()) {
            out << entry;
            out << "\n\n\n";
        }
    }

    std::cout << "ok\n";

    return 0;
}


int main(int argc, const char** argv) {
    try {
        return main_impl(argc, argv);
    } catch (std::exception const& e) {
        const boost::stacktrace::stacktrace*
            st = boost::get_error_info<traced>(e);

        if (st) { std::cerr << *st << std::endl; }

        std::cerr << "Got exception with type "
                  << cxxDemangle(typeid(e).name()) << ": " << e.what()
                  << std::endl;
        return 1;
    }
}
