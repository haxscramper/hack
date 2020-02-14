#include <iostream>
#include <numeric>
#include <string>
#include <vector>

#define let const auto
#define var auto

using Str = std::string;

template <typename T>
using Vec    = std::vector<T>;
using SVec   = Vec<Str>;
using SVecIt = SVec::iterator;

SVec split(Str str, Str delimiter) {
    size_t pos = 0;
    SVec   result;
    Str    token;
    while ((pos = str.find(delimiter)) != Str::npos) {
        token = str.substr(0, pos);
        str.erase(0, pos + delimiter.length());
        result.push_back(token);
    }
    result.push_back(str);
    return result;
}

Str join(SVecIt begin, SVecIt end, Str delimiter = " ") {
    Str res;
    while (begin != end) {
        res += *begin;
        begin++;
        if (begin != end) {
            res += delimiter;
        }
    }

    return res;
}

Str join(SVec vec, Str delimiter = " ") {
    return join(vec.begin(), vec.end(), delimiter);
}

int parseInt(Str str) {
    return std::atoi(str.c_str());
}

Str toStr(int num) {
    return std::to_string(num);
}

Str eval(SVec commands) {
    if (commands.size() == 3) {
        let lhs = parseInt(commands[0]);
        let rhs = parseInt(commands[2]);
        switch (commands[1][0]) {
            case '+': {
                return "sum: " + toStr(lhs + rhs);
            }
            case '-': {
                return "dif: " + toStr(lhs - rhs);
            }
            default: {
                return "undefined math operator: '" + commands[1] + "'";
            }
        }
    } else {
        return "other: " + join(commands);
    }
}

int main() {
    Str buf;

    while (getline(std::cin, buf)) {
        SVec commands = split(buf, " ");
        let  comm     = commands[0];
        if (comm == "echo") {
            let args = SVec(commands.begin() + 1, commands.end());
            std::cout << eval(args) << "\n";
        } else if (comm == "exit") {
            std::cout << "exit requested\n";
            exit(0);
        } else {
            std::cout << "Undefined command\n";
        }
    }

    return 0;
}
