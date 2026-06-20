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

        explicit NameTree(std::pmr::memory_resource* mr) : indexed(mr), named(mr) {}
    };

    NameTreeStore() : root_named_(&pool_) {}

    /// \brief Inserts a single event document into the tree.
    /// \param id Caller-provided dense event id.
    /// \param doc Parsed instantiation document.
    void insertEvent(EventId id, const rapidjson::Document& doc) {
        const auto& names = doc["names"];
        // The root <qualified> node's params are the main name chain.
        walkChain(names["params"], &root_named_, id.raw());
    }

    /// \brief Answers a query document, returning matching event ids.
    /// \param doc Query document, same schema, "<query_wildcard>" names
    ///        denote unconstrained positions.
    /// \return Bitmap view of matching events.
    SubsetCollection::BitmapView query(const rapidjson::Document& doc) {
        std::vector<SetId> constraints;
        queryChain(doc["names"]["params"], &root_named_, constraints);
        return intersectAll(constraints);
    }

    StringInterner&   interner() { return interner_; }
    SubsetCollection& sets() { return sets_; }

    /// \brief Accesses a node by descending a qualified-name chain.
    /// \param chain Sequence of interned names forming the path.
    /// \return Node pointer, or null if any segment is missing.
    NameTree* findNamed(const std::vector<StrId>& chain) {
        BranchingMap* level = &root_named_;
        NameTree*     node  = nullptr;
        for (StrId seg : chain) {
            auto it = level->find(seg);
            if (it == level->end()) { return nullptr; }
            node  = it->second;
            level = &node->named;
        }
        return node;
    }

  private:
    static constexpr const char* kWildcard = "<query_wildcard>";

    NameTree* makeNode(StrId name) {
        void* mem  = pool_.allocate(sizeof(NameTree), alignof(NameTree));
        auto* node = new (mem) NameTree(&pool_);
        node->name = name;
        node->set  = sets_.createSet();
        return node;
    }

    NameTree* childInsert(BranchingMap* level, StrId name) {
        auto it = level->find(name);
        if (it != level->end()) { return it->second; }
        NameTree* node = makeNode(name);
        level->emplace(name, node);
        return node;
    }

    bool isWildcard(const rapidjson::Value& nameNode) const {
        return std::string_view{nameNode["name"].GetString()} == kWildcard;
    }

    /// \brief Walks a params array as a qualified-name chain (insert).
    void walkChain(const rapidjson::Value& params, BranchingMap* level, uint32_t event) {
        for (const auto& seg : params.GetArray()) {
            const StrId name = interner_.intern(seg["name"]["name"].GetString());
            NameTree*   node = childInsert(level, name);
            sets_.add(node->set, event);
            walkParams(seg["params"], node, event);
            level = &node->named;
        }
    }

    /// \brief Walks a node's params as its template arguments (insert).
    void walkParams(const rapidjson::Value& params, NameTree* parent, uint32_t event) {
        const auto args = params.GetArray();
        while (parent->indexed.size() < args.Size()) { parent->indexed.emplace_back(); }

        for (rapidjson::SizeType k = 0; k < args.Size(); ++k) {
            const auto& arg = args[k];
            if (std::string_view{arg["name"]["name"].GetString()} == "<qualified>") {
                // Arg is itself a qualified chain a::b::...; thread it
                // directly into this positional branching level.
                walkChain(arg["params"], &parent->indexed[k], event);
            } else {
                const StrId name = interner_.intern(arg["name"]["name"].GetString());
                NameTree*   node = childInsert(&parent->indexed[k], name);
                sets_.add(node->set, event);
                walkParams(arg["params"], node, event);
            }
        }
    }

    void queryChain(
        const rapidjson::Value& params,
        BranchingMap*           level,
        std::vector<SetId>&     out) {
        BranchingMap* cur = level;
        for (const auto& seg : params.GetArray()) {
            const StrId name = interner_.intern(seg["name"]["name"].GetString());
            auto        it   = cur->find(name);
            if (it == cur->end()) {
                out.push_back(emptySet());
                return;
            }
            NameTree* node = it->second;
            out.push_back(node->set);
            queryParams(seg["params"], node, out);
            cur = &node->named;
        }
    }

    void queryParams(
        const rapidjson::Value& params,
        NameTree*               parent,
        std::vector<SetId>&     out) {
        const auto args = params.GetArray();
        for (rapidjson::SizeType k = 0; k < args.Size(); ++k) {
            const auto& arg = args[k];
            if (isWildcard(arg["name"])) { continue; }
            if (k >= parent->indexed.size()) {
                out.push_back(emptySet());
                return;
            }
            if (std::string_view{arg["name"]["name"].GetString()} == "<qualified>") {
                queryChain(arg["params"], &parent->indexed[k], out);
                continue;
            }
            const StrId name = interner_.intern(arg["name"]["name"].GetString());
            auto        it   = parent->indexed[k].find(name);
            if (it == parent->indexed[k].end()) {
                out.push_back(emptySet());
                return;
            }
            NameTree* node = it->second;
            out.push_back(node->set);
            queryParams(arg["params"], node, out);
        }
    }

    SetId emptySet() {
        if (empty_set_.raw() == 0 && !empty_inited_) {
            empty_set_    = sets_.createSet();
            empty_inited_ = true;
        }
        return empty_set_;
    }

    SubsetCollection::BitmapView intersectAll(const std::vector<SetId>& constraints) {
        if (constraints.empty()) { return sets_.values(emptySet()); }
        SubsetCollection::BitmapPtr acc = SubsetCollection::makeBitmap();
        roaring_bitmap_overwrite(acc.get(), sets_.values(constraints.front()).raw());
        for (std::size_t i = 1; i < constraints.size(); ++i) {
            roaring_bitmap_and_inplace(acc.get(), sets_.values(constraints[i]).raw());
        }
        return SubsetCollection::BitmapView{acc};
    }

    std::pmr::monotonic_buffer_resource pool_{1u << 20};
    StringInterner                      interner_;
    SubsetCollection                    sets_;
    BranchingMap                        root_named_;
    SetId                               empty_set_{0};
    bool                                empty_inited_ = false;
};
