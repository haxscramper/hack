// name_tree.hpp
#pragma once

#include <cstdint>
#include <memory>
#include <memory_resource>
#include <unordered_map>
#include <vector>

#include <rapidjson/document.h>

#include "set_collection.hpp"
#include "string_interner.hpp"

using EventId = StrongId<struct EventIdTag>;

class NameTreeStore {
  public:
    struct NameTree;

    /// \brief Map from interned name to child node, allocated from arena.
    using BranchingMap = std::pmr::unordered_map<StrId, NameTree*>;

    struct NameTree {
        StrId name;
        /// \brief Per-template-position branching alternatives.
        std::pmr::vector<BranchingMap> indexed;
        /// \brief Qualified-name continuation.
        BranchingMap named;
        /// \brief Events whose instantiation passes through this node.
        SetId set;

        explicit NameTree(std::pmr::memory_resource* mr);
    };

    NameTreeStore();

    /// \brief Inserts a single event document into the tree.
    /// \param id Caller-provided dense event id.
    /// \param doc Parsed instantiation document.
    void insertEvent(EventId id, const rapidjson::Document& doc);

    /// \brief Answers a query document, returning matching event ids.
    /// \param doc Query document, same schema, "<query_wildcard>" names
    ///        denote unconstrained positions.
    /// \return Bitmap view of matching events.
    SubsetCollection::BitmapView query(const rapidjson::Document& doc);

    StringInterner&   interner();
    SubsetCollection& sets();

    /// \brief Accesses a node by descending a qualified-name chain.
    /// \param chain Sequence of interned names forming the path.
    /// \return Node pointer, or null if any segment is missing.
    NameTree* findNamed(const std::vector<StrId>& chain);

  private:
    static constexpr const char* kWildcard = "<query_wildcard>";

    NameTree* makeNode(StrId name);
    NameTree* childInsert(BranchingMap* level, StrId name);
    bool      isWildcard(const rapidjson::Value& nameNode) const;

    /// \brief Walks a params array as a qualified-name chain (insert).
    void walkChain(const rapidjson::Value& params, BranchingMap* level, uint32_t event);

    /// \brief Walks a node's params as its template arguments (insert).
    void walkParams(const rapidjson::Value& params, NameTree* parent, uint32_t event);

    void queryChain(
        const rapidjson::Value& params,
        BranchingMap*           level,
        std::vector<SetId>&     out);

    void queryParams(
        const rapidjson::Value& params,
        NameTree*               parent,
        std::vector<SetId>&     out);

    SetId                        emptySet();
    SubsetCollection::BitmapView intersectAll(const std::vector<SetId>& constraints);

    std::pmr::monotonic_buffer_resource pool_{1u << 20};
    StringInterner                      interner_;
    SubsetCollection                    sets_;
    BranchingMap                        root_named_;
    SetId                               empty_set_{0};
    bool                                empty_inited_ = false;
};
