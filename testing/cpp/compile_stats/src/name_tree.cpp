// name_tree.cpp
#include "name_tree.hpp"

#include <new>
#include <string_view>

NameTreeStore::NameTree::NameTree(std::pmr::memory_resource* mr)
    : indexed(mr), named(mr) {}

NameTreeStore::NameTreeStore() : root_named_(&pool_) {}

void NameTreeStore::insertEvent(EventId id, const rapidjson::Document& doc) {
    const auto& names = doc["names"];
    // The root <qualified> node's params are the main name chain.
    walkChain(names["params"], &root_named_, id.raw());
}

SubsetCollection::View NameTreeStore::query(const rapidjson::Document& doc) {
    std::vector<SetId> constraints;
    queryChain(doc["names"]["params"], &root_named_, constraints);
    return intersectAll(constraints);
}

StringInterner& NameTreeStore::interner() { return interner_; }

SubsetCollection& NameTreeStore::sets() { return sets_; }

NameTreeStore::NameTree* NameTreeStore::findNamed(const std::vector<StrId>& chain) {
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

NameTreeStore::NameTree* NameTreeStore::makeNode(StrId name) {
    void* mem  = pool_.allocate(sizeof(NameTree), alignof(NameTree));
    auto* node = new (mem) NameTree(&pool_);
    node->name = name;
    node->set  = sets_.createSet();
    return node;
}

NameTreeStore::NameTree* NameTreeStore::childInsert(BranchingMap* level, StrId name) {
    auto it = level->find(name);
    if (it != level->end()) { return it->second; }
    NameTree* node = makeNode(name);
    level->emplace(name, node);
    return node;
}

bool NameTreeStore::isWildcard(const rapidjson::Value& nameNode) const {
    return std::string_view{nameNode["name"].GetString()} == kWildcard;
}

void NameTreeStore::walkChain(
    const rapidjson::Value& params,
    BranchingMap*           level,
    uint32_t                event) {
    for (const auto& seg : params.GetArray()) {
        const StrId name = interner_.intern(seg["name"]["name"].GetString());
        NameTree*   node = childInsert(level, name);
        sets_.add(node->set, event);
        walkParams(seg["params"], node, event);
        level = &node->named;
    }
}

void NameTreeStore::walkParams(
    const rapidjson::Value& params,
    NameTree*               parent,
    uint32_t                event) {
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

void NameTreeStore::queryChain(
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

void NameTreeStore::queryParams(
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

SetId NameTreeStore::emptySet() {
    if (empty_set_.raw() == 0 && !empty_inited_) {
        empty_set_    = sets_.createSet();
        empty_inited_ = true;
    }
    return empty_set_;
}

SubsetCollection::View NameTreeStore::intersectAll(
    const std::vector<SetId>& constraints) {
    if (constraints.empty()) { return sets_.values(emptySet()); }
    return sets_.intersection(constraints);
}
