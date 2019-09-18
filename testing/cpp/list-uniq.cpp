#include <iostream>
#include <optional>
#include <string>
#include <vector>

// TODO if both list exists check if `out` is increment of the `in`.
// If only `in` exists then build out. If none of the list exist
// return true;

class Logger
{
  public:
    ~Logger() {
        std::cout << "\b\n";
    }

    static Logger log() {
        return Logger();
    }

    template <class T>
    Logger& operator<<(T t) {
        std::cout << t << " ";
        return *this;
    }
};

#define LOG Logger::log()
#define logvar(var) LOG << #var << var;

struct List {
    std::string          content;
    std::optional<List*> next;

    bool operator==(const List& other) const {
        return this->content == other.content;
    }
};

using ListOpt = std::optional<List*>;

List add_to(List* in) {
    in->content += "_appended";
    return *in;
}

ListOpt make_list(std::vector<std::string> in) {
    ListOpt res;
    List*   prev;
    for (auto& str : in) {
        if (prev == nullptr) {
            LOG << "First element in list";
            prev = new List;
        } else {
            LOG << "Creating new element in list";
            auto tmp = new List;
            LOG << "Assign to tail";
            prev->next = tmp;
            prev       = prev->next.value();
        }
        prev->content = str;
        res           = prev;
    }

    return prev;
}

bool increment(ListOpt& in, ListOpt& out) {
    if (!in.has_value() && !out.has_value()) {
        LOG << "Two empty lists";
        return true;
    } else if (in.has_value() && !out.has_value()) {
        LOG << "In exists, out needs to be written to";
        out                  = new List();
        out.value()->content = add_to(in.value()).content;
        return true
               && increment(
                   in.value()->next, //
                   out.value()->next //
               );
    } else if (in.has_value() && out.has_value()) {
        LOG << "Both list exist and we need to check for their ";
        return (out.value()->content == add_to(in.value()).content)
               && increment(in, out);
    }
}


int main() {
    auto    in = make_list({"a", "b", "c"});
    ListOpt out;
    increment(in, out);
    LOG << "Done";
}
