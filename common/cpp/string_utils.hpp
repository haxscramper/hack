#include <string>
#include <vector>

#include "using.hpp"

Vec<Str> split(std::string str, std::string delimiter) {
    Vec<Str>    res;
    size_t      pos = 0;
    std::string token;
    while ((pos = str.find(delimiter)) != std::string::npos) {
        token = str.substr(0, pos);
        res.push_back(token);
        str.erase(0, pos + delimiter.length());
    }
    res.push_back(str);

    return res;
}
