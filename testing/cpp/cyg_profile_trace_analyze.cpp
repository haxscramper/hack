#include <iostream>
#include <cassert>
#include <unordered_map>
#include <sstream>
#include <vector>
#include <fstream>
#include "../../cpp_common.hpp"

struct CallEntry {
    bool enter;
    int  this_fn;
    int  callsite;
};
std::ostream& operator<<(std::ostream& os, CR<CallEntry> other) {
    os << "(" << std::boolalpha << other.enter << " " << other.this_fn
       << " " << other.callsite << ")";
    return os;
}

struct NameResolver {
    std::unordered_map<int, std::string> cache;

    std::string operator[](int addr) {
        if (cache.contains(addr)) {
            return cache[addr];
        } else {
            return "TODO";
        }
    }
};

struct CallTree {
    std::string           name;
    int                   callsite;
    std::vector<CallTree> subtrees;
    std::ostream& operator_shift_aux(std::ostream& os, int indent = 0)
        const {
        os << std::string(indent * 2, ' ');
        os << name << " at " << callsite << "\n";
        for (const auto& sub : subtrees) {
            sub.operator_shift_aux(os, indent + 1);
        }
        return os;
    }

    CallTree(Str _name, int call, CR<Vec<CallTree>> sub = {})
        : name(_name), callsite(call), subtrees(sub){};

    void push_back(CR<CallTree> other) { subtrees.push_back(other); }

    CallTree(
        NameResolver&                 resolve,
        std::vector<CallEntry> const& calls,
        int&                          index) {
        callsite = calls[index].callsite;
        name     = resolve[calls[index].this_fn];
        ++index;
        while (index < calls.size() && calls[index].enter) {
            subtrees.push_back(CallTree(resolve, calls, index));
        }
    }
};

std::ostream& operator<<(std::ostream& os, const CallTree& other) {
    return other.operator_shift_aux(os);
}


std::istream& operator>>(std::istream& is, CallEntry& entry) {
    char ch     = is.get();
    entry.enter = ch == '>';
    is >> entry.this_fn;
    is >> entry.callsite;
    return is;
}

void fill_traces(std::istream& is, std::vector<CallEntry>& vec) {
    std::string str;
    while (std::getline(is, str)) {
        std::stringstream stream{str};
        CallEntry         tmp;
        stream >> tmp;
        vec.push_back(tmp);
    }
}

int main(int argc, char** argv) {
    auto src_stream = std::ifstream{argv[1]};
    auto dst_stream = std::ifstream{argv[2]};

    std::vector<CallEntry> src_trace{}, dst_trace{};
    fill_traces(src_stream, src_trace);
    fill_traces(dst_stream, dst_trace);

    NameResolver resolve;

    int      index = 0;
    CallTree src{"main", 0};
    while (index < src_trace.size()) {
        src.push_back(CallTree{resolve, src_trace, index});
    }

    index = 0;
    CallTree dst{"main", 0};

    while (index < dst_trace.size()) {
        CallTree{resolve, dst_trace, index};
    }

    std::cout << "source " << src;
    std::cout << "destin " << dst;


    std::cout << "Trace difference computation completed found "
              << src_trace.size() << " soruce traces and "
              << dst_trace.size() << " destination traces\n";

    return 0;
}
