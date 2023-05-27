#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <nlohmann/json.hpp>

std::map<std::string, int> counts;

void countTypes(const nlohmann::json& j) {
    if (j.is_object()) {
        ++counts["object"];
        for (auto& element : j.items()) {
            countTypes(element.value());
        }
    } else if (j.is_array()) {
        ++counts["array"];
        for (auto& element : j) {
            countTypes(element);
        }
    } else if (j.is_string()) {
        ++counts["string"];
    } else if (j.is_boolean()) {
        ++counts["boolean"];
    } else if (j.is_number()) {
        ++counts["number"];
    } else if (j.is_null()) {
        ++counts["null"];
    }
}

int main() {
    std::ifstream file("/tmp/input.json");
    if (!file) {
        std::cerr << "Error opening file\n";
        return 1;
    }

    nlohmann::json j;
    file >> j;

    countTypes(j);

    std::cout << "Counts:\n";
    for (const auto& pair : counts) {
        std::cout << pair.first << ": " << pair.second << '\n';
    }

    std::cout << "size of json object: " << sizeof(j) << "\n";

    return 0;
}
