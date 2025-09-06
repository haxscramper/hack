#include <immer/box.hpp>
#include <immer/vector.hpp>
#include <lager/cursor.hpp>
#include <lager/event_loop/manual.hpp>
#include <lager/store.hpp>
#include <range/v3/algorithm/equal.hpp>
#include <spdlog/sinks/basic_file_sink.h>
#define LOGURU_USE_FMTLIB 1
#include <loguru.hpp>
#include <variant>

struct DiagramTreeNode {
  std::string id;
  std::string content;
  immer::vector<immer::box<DiagramTreeNode>> subnodes;

  std::string formatTreeHelper(const DiagramTreeNode &node, int depth,
                               const std::string &prefix, bool isLast) const {
    std::string result;
    result += prefix;
    result += isLast ? "└── " : "├── ";
    result += std::format("[{}] {}\n", node.id, node.content);

    auto childPrefix = prefix + (isLast ? "    " : "│   ");
    int childCount = node.subnodes.size();
    for (int i = 0; i < childCount; ++i) {
      result += formatTreeHelper(node.subnodes.at(i), depth + 1, childPrefix,
                                 i == childCount - 1);
    }
    return result;
  }

  std::string treeRepr() const {
    std::string result = std::format("[{}] {}\n", id, content);
    int childCount = subnodes.size();
    for (int i = 0; i < childCount; ++i) {
      result += formatTreeHelper(subnodes.at(i), 1, "", i == childCount - 1);
    }
    return result;
  }
};

using DiagramTreeBox = immer::box<DiagramTreeNode>;

struct AppTreeNode {
  std::string id;
  std::string content;
  immer::vector<immer::box<AppTreeNode>> children;

  bool operator==(AppTreeNode const &other) const {
    return id == other.id && content == other.content &&
           children.size() == other.children.size() &&
           ranges::equal(children, other.children);
  }

  std::string formatTreeHelper(const AppTreeNode &node, int depth,
                               const std::string &prefix, bool isLast) const {
    std::string result;
    result += prefix;
    result += isLast ? "└── " : "├── ";
    result += std::format("[{}] {}\n", node.id, node.content);

    auto childPrefix = prefix + (isLast ? "    " : "│   ");
    int childCount = node.children.size();
    for (int i = 0; i < childCount; ++i) {
      result += formatTreeHelper(node.children.at(i), depth + 1, childPrefix,
                                 i == childCount - 1);
    }
    return result;
  }

  std::string treeRepr() const {
    std::string result = std::format("[{}] {}\n", id, content);
    int childCount = children.size();
    for (int i = 0; i < childCount; ++i) {
      result += formatTreeHelper(children.at(i), 1, "", i == childCount - 1);
    }
    return result;
  }
};

using AppTreeBox = immer::box<AppTreeNode>;

struct AppState {
  AppTreeNode root;
};

struct InsertEdit {
  std::vector<int> path;
  int position;
  AppTreeNode node;
};

struct DeleteEdit {
  std::vector<int> path;
  int position;
};

struct ReplaceEdit {
  std::vector<int> path;
  AppTreeNode node;
};

using Edit = std::variant<InsertEdit, DeleteEdit, ReplaceEdit>;

template <> struct fmt::formatter<InsertEdit> {
  constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const InsertEdit &edit, FormatContext &ctx) const {
    std::string pathStr = "[";
    for (int i = 0; i < edit.path.size(); ++i) {
      if (i != 0)
        pathStr += ", ";
      pathStr += std::to_string(edit.path.at(i));
    }
    pathStr += "]";
    return fmt::format_to(ctx.out(),
                          "InsertEdit{{path={}, position={}, node.id={}}}",
                          pathStr, edit.position, edit.node.id);
  }
};

template <> struct fmt::formatter<DeleteEdit> {
  constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const DeleteEdit &edit, FormatContext &ctx) const {
    std::string pathStr = "[";
    for (int i = 0; i < edit.path.size(); ++i) {
      if (i != 0)
        pathStr += ", ";
      pathStr += std::to_string(edit.path.at(i));
    }
    pathStr += "]";
    return fmt::format_to(ctx.out(), "DeleteEdit{{path={}, position={}}}",
                          pathStr, edit.position);
  }
};

template <> struct fmt::formatter<ReplaceEdit> {
  constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const ReplaceEdit &edit, FormatContext &ctx) const {
    std::string pathStr = "[";
    for (int i = 0; i < edit.path.size(); ++i) {
      if (i != 0)
        pathStr += ", ";
      pathStr += std::to_string(edit.path.at(i));
    }
    pathStr += "]";
    return fmt::format_to(ctx.out(),
                          "ReplaceEdit{{path={}, node.id={}, node.content={}}}",
                          pathStr, edit.node.id, edit.node.content);
  }
};

template <> struct fmt::formatter<Edit> {
  constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const Edit &edit, FormatContext &ctx) const {
    return std::visit(
        [&ctx](const auto &e) { return fmt::format_to(ctx.out(), "{}", e); },
        edit);
  }
};

AppTreeNode convertToAppNode(const DiagramTreeNode &diagram) {
  LOG_SCOPE_F(1, "convertToAppNode");
  LOG_F(INFO, "Converting DiagramTreeNode id={} to AppTreeNode", diagram.id);
  auto children = immer::vector<immer::box<AppTreeNode>>{};
  for (const auto &subnode : diagram.subnodes) {
    children = children.push_back(convertToAppNode(subnode.get()));
  }
  LOG_F(INFO, "Converted node id={} with {} children", diagram.id,
        children.size());
  return AppTreeNode{.id = diagram.id,
                     .content = diagram.content,
                     .children =
                         immer::vector<immer::box<AppTreeNode>>{children}};
}

std::vector<Edit> generateEdits(const AppTreeNode &current,
                                const DiagramTreeNode &target,
                                std::vector<int> path = {}) {
  LOG_SCOPE_F(1, "Generate edits");
  LOG_F(INFO, "Generating edits for path size={}, current.id={}, target.id={}",
        path.size(), current.id, target.id);
  std::vector<Edit> edits;

  if (current.id != target.id || current.content != target.content) {
    LOG_F(INFO,
          "Node mismatch at path size={}: replacing id {} -> {}, "
          "content {} -> {}",
          path.size(), current.id, target.id, current.content, target.content);
    edits.push_back(
        ReplaceEdit{.path = path, .node = convertToAppNode(target)});
    return edits;
  }

  int currentSize = current.children.size();
  int targetSize = target.subnodes.size();
  LOG_F(INFO, "Comparing children: current={}, target={}", currentSize,
        targetSize);

  for (int i = 0; i < std::min(currentSize, targetSize); ++i) {
    auto childPath = path;
    childPath.push_back(i);
    LOG_F(INFO, "Processing child at index {}", i);
    auto childEdits =
        generateEdits(current.children.at(i), target.subnodes.at(i), childPath);
    edits.insert(edits.end(), childEdits.begin(), childEdits.end());
  }

  if (currentSize < targetSize) {
    for (int i = currentSize; i < targetSize; ++i) {
      LOG_F(INFO, "Inserting new node at position {} in path size={}", i,
            path.size());
      edits.push_back(
          InsertEdit{.path = path,
                     .position = i,
                     .node = convertToAppNode(target.subnodes.at(i))});
    }
  } else if (targetSize < currentSize) {
    for (int i = currentSize - 1; targetSize <= i; --i) {
      LOG_F(INFO, "Deleting node at position {} in path size={}", i,
            path.size());
      edits.push_back(DeleteEdit{.path = path, .position = i});
    }
  }

  LOG_F(INFO, "Generated {} edits for path size={}", edits.size(), path.size());
  return edits;
}

AppTreeNode *navigateToNode(AppTreeNode *node, const std::vector<int> &path) {
  LOG_F(INFO, "Navigating to node with path size={}", path.size());
  AppTreeNode *current = node;
  for (int index : path) {
    if (index < current->children.size()) {
      current = &const_cast<AppTreeNode &>(current->children.at(index).get());
      LOG_F(INFO, "Navigated to child at index {}", index);
    } else {
      LOG_F(ERROR, "Navigation failed: index {} out of bounds (size={})", index,
            current->children.size());
      return nullptr;
    }
  }
  return current;
}

AppTreeNode applyEditToNode(AppTreeNode node, const std::vector<int> &path,
                            std::function<AppTreeNode(AppTreeNode)> transform) {
  LOG_SCOPE_F(1, "Apply edits to node");
  LOG_F(INFO, "Applying edit to node at path size={}", path.size());
  if (path.empty()) {
    LOG_F(INFO, "Applying transform at target node");
    return transform(node);
  }

  int index = path.at(0);
  auto remainingPath = std::vector<int>(path.begin() + 1, path.end());
  LOG_F(INFO, "Recursing into child at index {}", index);

  auto newChildren = node.children;
  if (index < newChildren.size()) {
    newChildren =
        newChildren.set(index, applyEditToNode(newChildren.at(index),
                                               remainingPath, transform));
  } else {
    LOG_F(ERROR, "Cannot apply edit: index {} out of bounds", index);
  }

  node.children = immer::vector<AppTreeBox>{newChildren};
  return node;
}

AppState applyEdit(AppState state, const Edit &edit) {
  return std::visit(
      [&](const auto &e) -> AppState {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, InsertEdit>) {
          LOG_F(INFO, "Applying InsertEdit at path size={}, position={}",
                e.path.size(), e.position);
          state.root =
              applyEditToNode(state.root, e.path, [&](AppTreeNode node) {
                auto newChildren = node.children.push_back(e.node);
                node.children = immer::vector<AppTreeBox>{newChildren};
                LOG_F(INFO, "Inserted node, new children count={}",
                      newChildren.size());
                return node;
              });
        } else if constexpr (std::is_same_v<T, DeleteEdit>) {
          LOG_F(INFO, "Applying DeleteEdit at path size={}, position={}",
                e.path.size(), e.position);
          state.root =
              applyEditToNode(state.root, e.path, [&](AppTreeNode node) {
                if (e.position < node.children.size()) {
                  auto newChildren = immer::vector<AppTreeBox>{};
                  for (int i = 0; i < node.children.size(); ++i) {
                    if (i != e.position) {
                      newChildren = newChildren.push_back(node.children.at(i));
                    }
                  }
                  node.children = immer::vector<AppTreeBox>{newChildren};
                  LOG_F(INFO,
                        "Deleted node at position {}, new children count={}",
                        e.position, newChildren.size());
                } else {
                  LOG_F(ERROR, "Cannot delete: position {} out of bounds",
                        e.position);
                }
                return node;
              });
        } else if constexpr (std::is_same_v<T, ReplaceEdit>) {
          LOG_F(INFO, "Applying ReplaceEdit at path size={}", e.path.size());
          if (e.path.empty()) {
            LOG_F(INFO, "Replacing root node");
            state.root = e.node;
          } else {
            auto parentPath =
                std::vector<int>(e.path.begin(), e.path.end() - 1);
            int index = e.path.back();
            LOG_F(INFO, "Replacing child at index {} in parent path size={}",
                  index, parentPath.size());
            state.root =
                applyEditToNode(state.root, parentPath, [&](AppTreeNode node) {
                  if (index < node.children.size()) {
                    auto newChildren = node.children.set(index, e.node);
                    node.children =
                        immer::vector<immer::box<AppTreeNode>>{newChildren};
                    LOG_F(INFO, "Replaced child at index {}", index);
                  } else {
                    LOG_F(ERROR, "Cannot replace: index {} out of bounds",
                          index);
                  }
                  return node;
                });
          }
        }
        return state;
      },
      edit);
}

int main() {
  loguru::g_stderr_verbosity = loguru::Verbosity_OFF;
  loguru::g_preamble_date = false;
  loguru::g_preamble_time = false;
  loguru::g_preamble_uptime = false;
  loguru::g_preamble_thread = false;

  loguru::add_file("/tmp/tree_immer.log", loguru::Truncate,
                   loguru::Verbosity_MAX);

  LOG_F(INFO, "Starting DiagramTree application");

  auto diagramTrees = std::vector<DiagramTreeNode>{
      DiagramTreeNode{
          .id = "root1",
          .content = "Document 1",
          .subnodes =
              immer::vector<DiagramTreeBox>{
                  DiagramTreeNode{.id = "1.1",
                                  .content = "Section A",
                                  .subnodes = immer::vector<DiagramTreeBox>{}},
                  DiagramTreeNode{
                      .id = "1.2",
                      .content = "Section B",
                      .subnodes = immer::vector<DiagramTreeBox>{DiagramTreeNode{
                          .id = "1.2.1",
                          .content = "Subsection B1",
                          .subnodes = immer::vector<DiagramTreeBox>{}}}}}},

      DiagramTreeNode{
          .id = "root1",
          .content = "Document 1",
          .subnodes =
              immer::vector<DiagramTreeBox>{
                  DiagramTreeNode{.id = "1.1",
                                  .content = "Section A",
                                  .subnodes = immer::vector<DiagramTreeBox>{}},
                  DiagramTreeNode{
                      .id = "1.2",
                      .content = "Section B",
                      .subnodes =
                          immer::vector<DiagramTreeBox>{
                              DiagramTreeNode{.id = "1.2.1",
                                              .content =
                                                  "Subsection B1 -- changed",
                                              .subnodes = immer::vector<DiagramTreeBox>{}}}}}},

      DiagramTreeNode{
          .id = "root2",
          .content = "Document 2",
          .subnodes =
              immer::vector<DiagramTreeBox>{
                  DiagramTreeNode{.id = "2.1",
                                  .content = "Chapter 1",
                                  .subnodes =
                                      immer::vector<DiagramTreeBox>{}},
                  DiagramTreeNode{.id = "2.2",
                                  .content = "Chapter 2",
                                  .subnodes =
                                      immer::vector<DiagramTreeBox>{}},
                  DiagramTreeNode{.id = "2.3",
                                  .content = "Chapter 3",
                                  .subnodes =
                                      immer::vector<DiagramTreeBox>{}}}},
      DiagramTreeNode{
          .id = "root1",
          .content = "Document 1 Modified",
          .subnodes =
              immer::vector<DiagramTreeBox>{
                  DiagramTreeNode{.id = "1.1",
                                  .content = "Section A Updated",
                                  .subnodes =
                                      immer::vector<DiagramTreeBox>{}},
                  DiagramTreeNode{.id = "1.3",
                                  .content = "Section C",
                                  .subnodes =
                                      immer::vector<DiagramTreeBox>{}}}},

  };

  LOG_F(INFO, "Created {} diagram trees", diagramTrees.size());

  auto loop = lager::with_manual_event_loop{};
  AppState model = AppState{.root = convertToAppNode(diagramTrees.at(0))};
  auto reducer =
      lager::with_reducer([&](AppState const &model, Edit const &action) {
        return applyEdit(model, action);
      });

  auto store = lager::make_store<Edit>(model, loop, reducer);

  LOG_F(INFO, "Initialized lager store with first diagram tree");

  lager::reader<AppTreeNode> rootCursor =
      store.zoom(lager::lenses::attr(&AppState::root));

  lager::reader<std::string> rootIdCursor =
      rootCursor.zoom(lager::lenses::attr(&AppTreeNode::id));

  lager::reader<std::string> rootContentCursor =
      rootCursor.zoom(lager::lenses::attr(&AppTreeNode::content));

  lager::reader<immer::vector<AppTreeBox>> childrenCursor =
      rootCursor.zoom(lager::lenses::attr(&AppTreeNode::children));

  lager::watch(rootIdCursor, [](const std::string &id) {
    LOG_F(INFO, "Root ID changed to: {}", id);
  });

  lager::watch(rootContentCursor, [](const std::string &content) {
    LOG_F(INFO, "Root content changed to: {}", content);
  });

  lager::watch(childrenCursor, [](const auto &children) {
    LOG_F(INFO, "Children count changed to: {}", children.size());
    for (int i = 0; i < children.size(); ++i) {
      LOG_F(INFO, "  Child {}: {} - {}", i, children.at(i).get().id,
            children.at(i).get().content);
    }
  });

  lager::reader<std::optional<AppTreeBox>> deletedSubnodeCursor =
      rootCursor //
          .zoom(lager::lenses::attr(&AppTreeNode::children))
          .zoom(lager::lenses::at(0));

  lager::watch(deletedSubnodeCursor,
               [](std::optional<AppTreeBox> const &subnode) {
                 LOG_F(INFO, "subnode exists {}", subnode.has_value());
               });

  auto switch_to_tree = [&](int i) {
    VLOG_SCOPE_F(1, "switch_to_tree");
    LOG_F(INFO, "Switching to diagram tree {}", i);
    auto currentState = store.get();
    auto edits = generateEdits(currentState.root, diagramTrees.at(i));
    LOG_F(INFO, "Generated {} edits", edits.size());
    for (auto const &edit : edits) {
      LOG_F(INFO, "  {}", edit);
    }
    for (const auto &edit : edits) {
      LOG_F(INFO, "Applying edit to store");
      store.dispatch(edit);
    }
  };

  switch_to_tree(0);
  switch_to_tree(1);
  switch_to_tree(2);

  LOG_F(INFO, "Application completed");
  return 0;
}
