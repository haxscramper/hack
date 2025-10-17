
namespace hstd::log::expr {

class ValueProxyBool {
  public:
    enum class OpType
    {
        Value,
        And,
        Or,
        Not
    };

    struct ExprNode {
        OpType                    type;
        std::string               name;
        hstd::Func<bool()>        value;
        std::unique_ptr<ExprNode> left;
        std::unique_ptr<ExprNode> right;

        ExprNode(
            OpType                    t,
            const std::string&        n,
            hstd::Func<bool()> const& ptr)
            : type(t), name(n), value(ptr) {}

        ExprNode(OpType t, const std::string& n) : type(t), name(n) {}

        bool eval() const {
            switch (type) {
                case OpType::Value: return value();
                case OpType::And: return left->eval() && right->eval();
                case OpType::Or: return left->eval() || right->eval();
                case OpType::Not: return !left->eval();
            }
            return false;
        }
    };

  private:
    std::unique_ptr<ExprNode> root;

  public:
    ValueProxyBool(hstd::Func<bool()> const& ptr, const std::string& name)
        : root(std::make_unique<ExprNode>(OpType::Value, name, ptr)) {}

    template <typename T>
    static ValueProxyBool Init(
        hstd::Opt<T> const* ptr,
        const std::string&  name) {
        return ValueProxyBool([ptr]() { return ptr->has_value(); }, name);
    }

    static ValueProxyBool Init(bool const* ptr, const std::string& name) {
        return ValueProxyBool([ptr]() { return *ptr; }, name);
    }

    ValueProxyBool(std::unique_ptr<ExprNode> node)
        : root(std::move(node)) {}

    // Copy constructor
    ValueProxyBool(const ValueProxyBool& other) {
        root = copy_node(other.root.get());
    }

    // Copy assignment
    ValueProxyBool& operator=(const ValueProxyBool& other) {
        if (this != &other) { root = copy_node(other.root.get()); }
        return *this;
    }

    // Move constructor and assignment (default)
    ValueProxyBool(ValueProxyBool&&)            = default;
    ValueProxyBool& operator=(ValueProxyBool&&) = default;

    bool value() const { return root->eval(); }

    std::string format() const {
        struct GridCell {
            std::string name;
            bool        value;
            int         depth;
            bool        is_operator;
        };

        std::vector<GridCell> cells;

        auto get_max_depth = [](ExprNode const* node, auto& self) {
            if (node->type == OpType::Value) {
                return 0;
            } else if (node->type == OpType::Not) {
                return 1 + self(node->left.get(), self);
            } else {
                return 1
                     + std::max<int>(
                           self(node->left.get(), self),
                           self(node->right.get(), self));
            }
        };

        int max_depth = get_max_depth(root.get(), get_max_depth);

        auto collect_cells =
            [&](const ExprNode* node, int depth, auto& self) -> void {
            if (node->type == OpType::Value) {
                cells.push_back(
                    {node->name, node->eval(), max_depth - depth, false});
            } else if (node->type == OpType::Not) {
                self(node->left.get(), depth + 1, self);
                cells.push_back(
                    {node->name, node->eval(), max_depth - depth, true});
            } else { // And, Or
                self(node->left.get(), depth + 1, self);
                self(node->right.get(), depth + 1, self);
                cells.push_back(
                    {node->name, node->eval(), max_depth - depth, true});
            }
        };

        collect_cells(root.get(), 0, collect_cells);

        std::vector<std::vector<std::string>> grid;

        int total_cols = max_depth + 3;
        grid.resize(cells.size());
        for (auto& row : grid) { row.resize(total_cols, ""); }

        for (int i = 0; i < static_cast<int>(cells.size()); ++i) {
            const auto& cell     = cells.at(i);
            int         base_col = cell.depth;

            grid.at(i).at(base_col)     = cell.name;
            grid.at(i).at(base_col + 1) = "=";
            grid.at(i).at(base_col + 2) = cell.value ? "true" : "false";
        }

        std::vector<int> col_widths(total_cols, 0);
        for (const auto& row : grid) {
            for (int col = 0; col < static_cast<int>(row.size()); ++col) {
                if (!row.at(col).empty()) {
                    col_widths.at(col) = std::max(
                        col_widths.at(col),
                        static_cast<int>(row.at(col).length()));
                }
            }
        }

        std::string result;
        for (int row = 0; row < static_cast<int>(grid.size()); ++row) {
            std::string line;

            for (int col = 0; col < static_cast<int>(grid.at(row).size());
                 ++col) {
                const std::string& cell = grid.at(row).at(col);

                if (!cell.empty()) {
                    if (col > 0) { line += " "; }
                    line += std::format(
                        "{:<{}}", cell, col_widths.at(col));
                } else if (col_widths.at(col) > 0) {
                    if (col > 0) { line += " "; }
                    line += std::string(col_widths.at(col), ' ');
                }
            }

            result += line;
            if (row < static_cast<int>(grid.size()) - 1) {
                result += "\n";
            }
        }

        return result;
    }

    ValueProxyBool operator&&(const ValueProxyBool& other) const {
        auto and_node   = std::make_unique<ExprNode>(OpType::And, "&&");
        and_node->left  = copy_node(root.get());
        and_node->right = copy_node(other.root.get());
        return ValueProxyBool(std::move(and_node));
    }

    ValueProxyBool operator||(const ValueProxyBool& other) const {
        auto or_node   = std::make_unique<ExprNode>(OpType::Or, "||");
        or_node->left  = copy_node(root.get());
        or_node->right = copy_node(other.root.get());
        return ValueProxyBool(std::move(or_node));
    }

    ValueProxyBool operator!() const {
        auto not_node  = std::make_unique<ExprNode>(OpType::Not, "!");
        not_node->left = copy_node(root.get());
        return ValueProxyBool(std::move(not_node));
    }

    // Prevent implicit conversion to bool
    explicit operator bool() const = delete;

    std::unique_ptr<ExprNode> copy_node(const ExprNode* node) const {
        if (!node) { return nullptr; }

        auto new_node = std::make_unique<ExprNode>(
            node->type, node->name, node->value);
        if (node->left) { new_node->left = copy_node(node->left.get()); }
        if (node->right) {
            new_node->right = copy_node(node->right.get());
        }
        return new_node;
    }
};

} // namespace hstd::log::expr

#define HSLOG_DEBUG_EXPR_BOOL(___expr)                                    \
    hstd::log::expr::ValueProxyBool::Init(&(___expr), #___expr)

#define HSLOG_DEBUG_EXPR_VAL(___expr)                                     \
    ({                                                                    \
        auto evaluator = (___expr);                                       \
        HSLOG_DEBUG("{}", evaluator.format());                            \
        evaluator.value();                                                \
    })

#define HSLOG_DEBUG_EXPR_VAL1(__expr)                                     \
    HSLOG_DEBUG_EXPR_VAL(HSLOG_DEBUG_EXPR_BOOL(__expr))

#define HSLOG_DEBUG_EXPR(__expr)                                          \
    ({                                                                    \
        auto const& res = __expr;                                         \
        HSLOG_DEBUG(                                                      \
            "{} = {}", #__expr, hstd::escape_literal(hstd::fmt1(res)));   \
        res;                                                              \
    })
