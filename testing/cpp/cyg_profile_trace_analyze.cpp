#include <iostream>
#include <cassert>
#include <unordered_map>
#include <sstream>
#include <vector>
#include <fstream>
#include <cstdio>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <array>

#include "../../cpp_common.hpp"


std::string exec(CR<Str> cmd) {
    std::array<char, 128>                    buffer;
    std::string                              result;
    std::unique_ptr<FILE, decltype(&pclose)> pipe(
        popen(cmd.c_str(), "r"), pclose);

    if (!pipe) { throw std::runtime_error("popen() failed!"); }

    while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
        result += buffer.data();
    }

    return result;
}


using Addr = std::size_t;

struct CallEntry {
    bool enter;
    Addr this_fn;
    Addr callsite;
    Str  name;
};
std::ostream& operator<<(std::ostream& os, CR<CallEntry> other) {
    std::ios::fmtflags flags{os.flags()};
    os << "("                                  //
       << std::boolalpha << other.enter << " " //
       << std::hex << other.this_fn << " "     //
       << std::hex << other.callsite << ")";

    os.flags(flags);
    return os;
}

const std::string WHITESPACE = " \n\r\t\f\v";

std::string ltrim(const std::string& s) {
    size_t start = s.find_first_not_of(WHITESPACE);
    return (start == std::string::npos) ? "" : s.substr(start);
}

std::string rtrim(const std::string& s) {
    size_t end = s.find_last_not_of(WHITESPACE);
    return (end == std::string::npos) ? "" : s.substr(0, end + 1);
}

std::string trim(const std::string& s) { return rtrim(ltrim(s)); }

struct NameResolver {
    std::unordered_map<Addr, std::string> cache;
    Str                                   binary;

    inline NameResolver(CR<Str> _binary) : binary{_binary} {}

    std::string get(Addr addr, Str name) {
        if (cache.contains(addr)) {
            return cache[addr];
        } else {
            std::ostringstream cmd;
            cmd << "echo '" << name << "' | c++filt";
            auto name   = trim(exec(cmd.str()));
            cache[addr] = name;
            return name;
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
        os << name << "\n";
        for (const auto& sub : subtrees) {
            sub.operator_shift_aux(os, indent + 1);
        }
        return os;
    }

    CallTree(Str _name, int call, CR<Vec<CallTree>> sub = {})
        : name(_name), callsite(call), subtrees(sub){};

    void push_back(CR<CallTree> other) { subtrees.push_back(other); }

    CallTree(NameResolver& resolve, CR<CallEntry> entry) {
        callsite = entry.callsite;
        name     = resolve.get(entry.this_fn, entry.name);
    }
};

std::ostream& operator<<(std::ostream& os, const CallTree& other) {
    return other.operator_shift_aux(os);
}


std::istream& operator>>(std::istream& is, CallEntry& entry) {
    char ch     = is.get();
    entry.enter = ch == '>';
    is >> std::hex >> entry.this_fn;
    is >> std::hex >> entry.callsite;
    if (entry.enter) { is >> entry.name; }
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

CallTree parse_traces(
    NameResolver&  resolve,
    Vec<CallEntry> calls,
    int&           idx) {
    CallTree result{resolve, calls[idx]};
    ++idx;
    while (calls[idx].enter) {
        result.push_back(parse_traces(resolve, calls, idx));
    }

    return result;
}

int main(int argc, char** argv) {
    assert(
        argc == 4 &&
        "Missing original binary name. Usage: analyze.bin file1.log "
        "file2.log binary.bin");

    auto src_stream = std::ifstream{argv[1]};
    auto dst_stream = std::ifstream{argv[2]};

    std::vector<CallEntry> src_trace{}, dst_trace{};
    fill_traces(src_stream, src_trace);
    fill_traces(dst_stream, dst_trace);

    NameResolver resolve{argv[3]};

    int      index = 0;
    CallTree src{"main", 0};
    while (index < src_trace.size()) {
        src.push_back(parse_traces(resolve, src_trace, index));
    }

    index = 0;
    CallTree dst{"main", 0};

    while (index < dst_trace.size()) {
        dst.push_back(parse_traces(resolve, dst_trace, index));
    }

    std::cout << "source " << src;
    std::cout << "destin " << dst;


    std::cout << "Trace difference computation completed found "
              << src_trace.size() << " soruce traces and "
              << dst_trace.size() << " destination traces\n";

    return 0;
}
