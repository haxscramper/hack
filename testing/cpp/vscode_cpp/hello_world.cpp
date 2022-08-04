#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <iterator>
#include <map>
#include <tuple>
#include <algorithm>
#include <variant>

template <typename T>
std::ostream& operator<<(std::ostream& out, const std::vector<T>& in) {
    out << "[";
    bool first = true;
    for (const auto& it : in) {
        if (!first) { out << ", "; }
        first = false;
        out << it;
    }
    out << "]";
    return out;
}

template <typename K, typename V>
std::ostream& operator<<(std::ostream& out, const std::map<K, V>& in) {
    out << "{";
    bool first = true;
    for (const auto& it : in) {
        if (!first) { out << ", "; }
        first = false;
        out << it.first;
        out << " = ", out << it.second;
    }
    out << "}";
    return out;
}

template <typename T>
struct reversion_wrapper {
    T& iterable;
};

template <typename T>
auto begin(reversion_wrapper<T> w) {
    return std::rbegin(w.iterable);
}

template <typename T>
auto end(reversion_wrapper<T> w) {
    return std::rend(w.iterable);
}

template <typename T>
reversion_wrapper<T> reverse(T&& iterable) {
    return {iterable};
}

std::string get_line(std::istringstream& stream) {
    std::string result;
    std::getline(stream, result);
    return result;
}

char getchar(std::istringstream& stream) {
    char ch;
    stream.get(ch);
    return ch;
}

/// Split cell name "COLUMN123" into "COLUMN" and row index
std::tuple<std::string, int> split_cell(const std::string& name) {
    int split = std::distance(
        name.cbegin(),
        std::partition_point(name.cbegin(), name.cend(), [](char it) {
            return std::isalpha(it);
        }));

    return {name.substr(0, split), std::stoi(name.substr(split))};
}

int column_to_idx(const std::string& name) {
    const int radix    = (int)('z' - 'a');
    int       position = 0;
    int       result;
    for (auto ch : reverse(name)) {
        result += position * radix + (int)(toupper(ch) - 'A');
        ++position;
    }
    return result;
}

struct position {
    int row, col;
};

position to_position(const std::string& cell) {
    auto [column, row] = split_cell(cell);
    return {row, column_to_idx(column)};
}

struct table {

    struct cell {
        int width, height;
    };

    enum class result_kind
    {
        merge_ok,
        failed_unaligned,
        failed_elementary
    };

    cell get_cell(const position& pos) { return cells[pos.row][pos.col]; }

    result_kind merge(const std::string& cell1, const std::string& cell2) {
        auto source = to_position(cell1);
        auto target = to_position(cell2);
    }

    std::variant<result_kind, int> split(const std::string& cell) {
        int new_split = 0;

        auto pos = to_position(cell);


        return new_split;
    }

  private:
    // [rows][columns] -> cell sizes
    std::map<int, std::map<int, cell>> cells;
};

int main() {
    std::string example{R"(
5 11
+--+------+
|  |      |
+--+----+-+
|       | |
+-------+-+
2
1 1
3
2 4 1
6
merge B1 A2
merge A2 C2
merge A2 B2
split A1
split C2
merge A1 A2
  )"};

    std::istringstream in{example};

    int row_count{}, column_count{};

    in >> row_count >> column_count;
    in.get();

    // list of rows with indices of the `|` in each row
    std::vector<std::vector<int>> column_positions{};
    for (int row = 0; row < row_count; ++row) {
        // separator rows don't have any new information, so they are
        // ignored
        bool separator = false;
        while (in.peek() == '+' || in.peek() == '-') {
            in.get();
            separator = true;
        }
        if (separator) {
            // skip trailing newline
            in.get();
            continue;
        }

        // if this is a proper content row add new vector with `|`
        // positions
        column_positions.push_back({});
        int position = 0;
        for (int i = 0; i < column_count + 1; ++i) {
            if (in.peek() == '|') {
                // collect positions of the columns
                column_positions.back().push_back(position);
            }
            ++position;
            in.get();
        }
        // trailing newline is handled by the last `in.get()` call
    }
    std::cout << column_positions;
}