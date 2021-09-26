#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <boost/wave.hpp>
#include <boost/wave/cpplexer/cpp_lex_iterator.hpp> // lexer class
#include <boost/wave/cpplexer/cpp_lex_token.hpp>    // token class

using namespace boost::wave;

struct WaveHooksImpl;

using WaveTokenT = cpplexer::lex_token<>;

using WaveTokenList = std::list<
    WaveTokenT,
    boost::fast_pool_allocator<
        cpplexer::lex_token<>,
        boost::default_user_allocator_new_delete>>;

using WaveContextImpl = context<
    std::string::iterator,
    cpplexer::lex_iterator<WaveTokenT>,
    iteration_context_policies::load_file_to_string,
    WaveHooksImpl>;

template <typename ResultT, typename... Arguments>
struct method_impl {
    ResultT (*impl)(Arguments..., void* env);
    void* env;

    method_impl() {
        env  = nullptr;
        impl = nullptr;
    };


    method_impl(
        ResultT (*_impl)(Arguments..., void* env),
        void* _env = nullptr)
        : impl(_impl), env(_env) {
    }

    ResultT operator()(Arguments... arguments) {
        return (*impl)(arguments..., env);
    }
};

enum class EntryHandling
{
    skip,
    process,
    raise
};

const char* to_string(EntryHandling handling) {
    switch (handling) {
        case EntryHandling::skip: return "skip";
        case EntryHandling::process: return "skip";
        case EntryHandling::raise: return "skip";
    }
}

using FoundWarningDirectiveCbType = method_impl<
    EntryHandling,
    WaveContextImpl const&,
    WaveTokenList const&>;


struct WaveHooksImpl : public context_policies::default_preprocessing_hooks {
    FoundWarningDirectiveCbType found_warning_directive_impl;

    inline void set_found_warning_directive_impl(
        FoundWarningDirectiveCbType impl) {
        found_warning_directive_impl = impl;
    }


    inline bool found_warning_directive(
        WaveContextImpl const& ctx,
        WaveTokenList const&    message) {
        auto handling = found_warning_directive_impl(ctx, message);

        switch (handling) {
            case EntryHandling::raise: return false;
            case EntryHandling::skip: return true;
            default:
                throw std::logic_error(
                    std::string("'found_warning_directive_impl' returned "
                                "unexpected "
                                "entry handling value - wanted 'raise' or "
                                "'skip', but "
                                "got ")
                    + to_string(handling));
        }
    }
};

EntryHandling found_warning_directive_impl(
    WaveContextImpl const& ctx,
    WaveTokenList const&    message,
    void*               env) {
    std::cout << "Found warning directive with message [["
              << util::impl::as_string(message) << "]]\n";
    return EntryHandling::skip;
}

int main(int argc [[maybe_unused]], char* argv [[maybe_unused]][]) {
    std::string instring
        = "#pragma once\n"
          "#warning \"asdfasdf\"\n"
          "#warning \"zzzz\"\n";


    WaveHooksImpl hooks;
    hooks.set_found_warning_directive_impl(&found_warning_directive_impl);

    WaveContextImpl ctx(
        instring.begin(), instring.end(), "in_file.hpp", hooks);

    util::file_position_type    current_position;
    WaveContextImpl::iterator_type first = ctx.begin();
    WaveContextImpl::iterator_type last  = ctx.end();

    try {
        while (first != last) {
            current_position = (*first).get_position();
            std::cout << (*first).get_value();
            ++first;
        }
    } catch (cpplexer::lexing_exception const& e) {
        std::cerr << e.file_name() << "(" << e.line_no()
                  << "): " << e.description() << std::endl;

    } catch (cpp_exception const& e) {
        std::cerr << e.file_name() << "(" << e.line_no()
                  << "): " << e.description() << std::endl;
        return 2;
    }
}
