#include <string>
#include <utility>
#include <iostream>
#include <memory>
#include <vector>
#include <functional>
#include <variant>

#include <nlohmann/json.hpp>

#define BOOST_PP_VARIADICS
#include <boost/preprocessor.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/seq/fold_left.hpp>
#include <boost/preprocessor/variadic/to_seq.hpp>
#include <boost/preprocessor/seq/cat.hpp>

using json = nlohmann::json;

template <typename T>
using Vec = std::vector<T>;

template <typename T>
using Ptr = std::shared_ptr<T>;

struct Org : public std::enable_shared_from_this<Org> {
    enum class Kind
    {
        Link,
        Word,
        Bold,
        Paragraph,
    };

#define every_kind(__impl)                                                \
    __impl(Link) __impl(Word) __impl(Bold) __impl(Paragraph)

    std::string strKind() const {
#define __case(__Kind)                                                    \
    case Kind::__Kind: return #__Kind;

        switch (getKind()) { every_kind(__case); }

#undef __case
    }

    virtual Kind  getKind() const = 0;
    Vec<Ptr<Org>> subnodes;

    template <typename T>
    Ptr<T> as() {
        Ptr<T> result = std::static_pointer_cast<T>(shared_from_this());
        return result;
    }
};


struct Word : Org {
    virtual Kind getKind() const override { return Kind::Word; }
};

struct Link : Org {
    Ptr<Word>    target;
    Ptr<Word>    description;
    virtual Kind getKind() const override { return Kind::Link; }
};

struct Paragraph : Org {
    virtual Kind getKind() const override { return Kind::Paragraph; }
};

struct Bold : Org {
    virtual Kind getKind() const override { return Kind::Bold; }
};

struct finally {
    std::function<void(void)> action;
    explicit finally(std::function<void(void)> _action)
        : action(_action) {}

    template <typename T>
    static finally init(
        std::function<void(T const&)> _action,
        T const&                      value) {
        return finally([value, _action]() { _action(value); });
    }

    ~finally() { action(); }
};


template <typename V, typename R>
struct Visit {
    template <typename T>
    void visitField(R& arg, const char* name, T const& val) {}
    void visitSubnode(R&, int, Ptr<Org> const& val) { visit(val); }
    R    newRes(Ptr<Org> const) { return R{}; }

    void pushVisit(Ptr<Org> const) {}
    void popVisit(Ptr<Org> const) {}

    R visit(Ptr<Org> const arg) {
        switch (arg->getKind()) {
#define __case(__Kind)                                                    \
    case Org::Kind::__Kind: {                                             \
        Ptr<__Kind> tmp = arg->as<__Kind>();                              \
        _this()->pushVisit(tmp);                                          \
        finally res{[&, this]() { _this()->popVisit(tmp); }};             \
        return _this()->visit##__Kind(tmp);                               \
    }
            every_kind(__case)

#undef __case
        }
    }

    void eachSub(R& res, Ptr<Org> org) {
        int idx = 0;
        for (const auto& it : org->subnodes) {
            _this()->visitSubnode(res, idx, it);
            ++idx;
        }
    }

    V* _this() { return static_cast<V*>(this); }

#define __field(res, obj, name) _this()->visitField(res, #name, obj->name);
#define __only_sub(__Kind)                                                \
    R visit##__Kind(Ptr<__Kind> p) {                                      \
        R tmp = _this()->newRes(p);                                       \
        eachSub(tmp, p);                                                  \
        return tmp;                                                       \
    }

    R visitLink(Ptr<Link> link) {
        R res = _this()->newRes(link);
        __field(res, link, target);
        __field(res, link, description);
        eachSub(res, link);
        return res;
    }

    __only_sub(Word);
    __only_sub(Bold);
    __only_sub(Paragraph);


#undef __only_sub
#undef __field

#define __visit(__Kind)                                                   \
    R operator()(Ptr<__Kind> const word) { return visit##__Kind(word); }

    every_kind(__visit)

#undef __visit
};

#define OP(s, state, x) BOOST_PP_CAT(state, x)

#define COMPOSE(...)                                                      \
    BOOST_PP_SEQ_FOLD_LEFT(                                               \
        OP,                                                               \
        BOOST_PP_SEQ_HEAD(BOOST_PP_VARIADIC_TO_SEQ(__VA_ARGS__)),         \
        BOOST_PP_SEQ_TAIL(BOOST_PP_VARIADIC_TO_SEQ(__VA_ARGS__)))

#define COMMA ,
#define skip1(op, ...) __VA_ARGS__
#define skip(op, ...) skip1(op)
#define __id(I) , Ptr<I>
#define __variant() std::variant<skip(every_kind(__id))>;

using OrgVariant = __variant();

#undef __id
#undef skip
#undef skip1
#undef COMMA
#undef __variant

OrgVariant asVariant(Ptr<Org> org) {
#define __case(__Kind)                                                    \
    case Org::Kind::__Kind: return org->as<__Kind>();

    switch (org->getKind()) { every_kind(__case) }

#undef __case
}

struct JsonVisitor : Visit<JsonVisitor, json> {
    json newRes(Ptr<Org> o) {
        json res    = json::object();
        res["kind"] = o->strKind();
        return res;
    }

    void visitField(json& arg, char const* name, Ptr<Org> const value) {
        arg[name] = visit(value);
    }
};

struct DotVisitor : Visit<DotVisitor, std::string> {
    std::vector<std::string> stack;

    void pushVisit(Ptr<Org> org) {
        stack.push_back(std::to_string((unsigned long long)org.get()));
    }

    void popVisit(Ptr<Org> org) { stack.pop_back(); }

    void visitSubnode(std::string& arg, int idx, Ptr<Org> const value) {
        visit(value);
    }

    void visitField(
        std::string&   arg,
        char const*    name,
        Ptr<Org> const value) {
        stack.back() += name;
    }

#define __visit(__Kind)                                                   \
    std::string visit##__Kind(Ptr<__Kind> const org) {                    \
        return stack.back();                                              \
    }

    every_kind(__visit)

#undef __visit
};


template <typename T>
std::shared_ptr<T> Sem() {
    return std::make_shared<T>();
}

int main() {
    auto l         = Sem<Link>();
    l->description = Sem<Word>();
    l->target      = Sem<Word>();

    std::string link = to_string(JsonVisitor{}.visit(l));
    std::string dot  = DotVisitor{}.visit(l);

    std::cout << "[" << link << "] [" << dot << "]\n";
}
