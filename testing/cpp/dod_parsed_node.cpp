#include <iostream>
#include <cassert>
#include <vector>
#include <string>

struct TreeStore;

enum TreeKind
{
    Token,
    Nested
};

struct Tree {
    static TreeStore* store;
    TreeKind          kind;
    int               value;

    static Tree Token(int index = 0) {
        return Tree{.kind = TreeKind::Token, .value = index};
    }

    static Tree Nested() { return Tree{.kind = TreeKind::Nested}; }

    void extend(int current, int nested) { value = nested - current; }
};

TreeStore* Tree::store;

struct TreeStore {
    std::vector<Tree> stored;

    int push(Tree const& tree) {
        int index = stored.size();
        stored.push_back(tree);
        return index;
    }

    int   current() const { return stored.size() - 1; }
    Tree& operator[](int index) { return stored.at(index); }
};

struct Lexer {
    std::string_view view;

    int  pos;
    void next(int count = 1) { ++pos; }
    char get(int offset = 0) const { return view.at(pos + offset); }
    bool at(char c) const { return get() == c; }
    void skip(char c) {
        assert(at(c));
        next(1);
    }
};

std::ostream& operator<<(std::ostream& os, Lexer const& value) {
    os << value.pos << ":" << value.get();
    return os;
}

void parse(TreeStore& store, Lexer& lexer, int level) {
    std::cout << std::string(level, ' ');
    if (lexer.at('{')) {
        std::cout << "open\n";
        int result = store.push(Tree::Nested());
        lexer.skip('{');
        while (!lexer.at('}')) {
            parse(store, lexer, level + 1);
        }

        store[result].extend(result, store.current());
        lexer.skip('}');
    } else {
        std::cout << "token group\n";
        while (!lexer.at('}')) {
            store.push(Tree::Token(lexer.pos));
            lexer.next();
        }
    }
}

int main() {
    std::string base = "{{{{732237}}}{1323}}";
    Lexer       lexer{.view = std::string_view(base.data(), base.size())};
    TreeStore   store;
    Tree::store = &store;
    parse(store, lexer, 0);

    int index = 0;
    for (const auto& node : store.stored) {
        if (node.kind == Token) {
            std::cout << "[" << index << "] " << node.value << " '"
                      << base[node.value] << "'\n";
        } else {
            std::cout << "[" << index << "] .." << node.value << " -> ["
                      << index << "/" << index + node.value << "]\n";
        }
        ++index;
    }

    std::cout << "done" << std::endl;
    return 0;
}
