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

    Opt<Str> outArg;
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
    }
}

void operator>>(const YAML::Node& node, Str& str) { str = node.as<Str>(); }

void operator>>(const YAML::Node& node, UserWrapRule& rule) {
    // It looks like YAML parser documentation is outdated for ~half a
    // decade if not more (there is an issue dating back 2015) - it says
    // there is a `.FindValue` method, which is not the case. For constant
    // nodes `operator[]` returns 'null' node, for mutable it might insert
    // a new node. This API has the same pitfall as `std::map::operator[]`,
    // but short of iterating over all child nodes I don't see any
    // reasonable way to find a matching solution.
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

class ConvertPusher : public MatchFinder::MatchCallback
{
    Vec<Str>               wrapped;   /// Wrapped node results
    Vec<CustomizerCollect> collector; /// Store results of the
                                      /// customized rules
    MatchFinder finder;               /// Finder for customizer rules

  public:
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

    virtual void run(const MatchFinder::MatchResult& Result) {
        auto func = Result.Nodes.getNodeAs<FunctionDecl>("function");

        for (auto& it : collector) {
            it.bindings.clear();
        }

        finder.match<Decl>(
            *Result.Nodes.getNodeAs<Decl>("function"),
            func->getASTContext());

        auto matched = getMatchedRules();

        if (0 < matched.size()) {
            llvm::outs()
                << "found declaration matched by the explicit rule";

        } else {
            llvm::outs() << "found declaration " << func->getName()
                         << "\n";
            for (const auto& param : func->parameters()) {
                llvm::outs() << "  arg ";
                param->print(llvm::outs());
                llvm::outs() << "\n";
                // << param->getName() << " "
                //              << param->getOriginalType() << "\n";
            }
        }
    }
};


int main(int argc, char** argv) {
    Vec<Str> input;
    Vec<Str> extra;

    bool extra_section = false;
    for (int i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "--") == 0) {
            extra_section = true;
            continue;
        }

        if (extra_section) {
            extra.push_back(argv[i]);
        } else {
            input.push_back(argv[i]);
        }
    }

    Vec<stdf::path> files;

    for (const auto& dir : input) {
        if (dir.find("email.h") != Str::npos) {
            continue;
        } else if (stdf::is_regular_file(dir)) {
            files.push_back(dir);
        } else {
            for (const auto& file : stdf::directory_iterator(dir)) {
                if (stdf::is_regular_file(file)) { files.push_back(file); }
            }
        }
    }

    Vec<const char*> clang_argv;
    for (const auto& path : files) {
        clang_argv.push_back(path.c_str());
    }

    for (const auto& opt : extra) {
        clang_argv.push_back(opt.c_str());
    }


    llvm::cl::OptionCategory category("genwrapper");

    int          cl_argc = clang_argv.size();
    const char** cl_argv = clang_argv.data();

    auto ExpectedParser = CommonOptionsParser::create(
        cl_argc, cl_argv, category);

    if (!ExpectedParser) {
        // Fail gracefully for unsupported options.
        llvm::errs() << ExpectedParser.takeError();
        return 1;
    }

    CommonOptionsParser& OptionsParser = ExpectedParser.get();
    ClangTool            Tool(
        OptionsParser.getCompilations(),
        OptionsParser.getSourcePathList());

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
