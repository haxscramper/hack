// name_tree.cpp
#include "name_tree.hpp"

#include <new>
#include <string_view>

NameTreeStore::NameTree::NameTree(std::pmr::memory_resource* mr)
    : indexed(mr), named(mr) {}

NameTreeStore::NameTreeStore() : root_named_(&pool_) {}

std::vector<StrId> NameTreeStore::insertEvent(
    EventId                    id,
    const rapidjson::Document& doc,
    VisitMode                  mode) {
    BranchingMap* root = (mode == VisitMode::Insert) ? &root_named_ : nullptr;
    return walkChain(doc["names"]["params"], root, id.raw(), mode);
}


SubsetCollection::BitmapView NameTreeStore::query(const rapidjson::Document& doc) {
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

NameTreeStore::NameTree* NameTreeStore::nestedInsert(BranchingMap* level, StrId name) {
    auto it = level->find(name);
    if (it != level->end()) { return it->second; }
    NameTree* node = makeNode(name);
    level->emplace(name, node);
    return node;
}

bool NameTreeStore::isWildcard(const rapidjson::Value& nameNode) const {
    return std::string_view{nameNode["name"].GetString()} == kWildcard;
}

std::vector<StrId> NameTreeStore::walkChain(
    const rapidjson::Value& params,
    BranchingMap*           level,
    uint32_t                event,
    VisitMode               mode) {
    std::vector<StrId> out;
    walkChainInto(params, level, event, mode, out);
    return out;
}

void NameTreeStore::walkChainInto(
    const rapidjson::Value& params,
    BranchingMap*           level,
    uint32_t                event,
    VisitMode               mode,
    std::vector<StrId>&     out) {
    for (const auto& seg : params.GetArray()) {
        const StrId name = interner_.intern(seg["name"]["name"].GetString());
        out.push_back(name);

        const auto& segParams = seg["params"];
        const bool  hasParams = !segParams.GetArray().Empty();

        if (mode == VisitMode::Insert) {
            NameTree* node = nestedInsert(level, name);
            sets_.add(node->set, event);
            walkParams(segParams, node, event, mode, out);
            level = &node->named;
            continue;
        }

        if (mode == VisitMode::SequenceOnly) {
            walkParams(segParams, nullptr, event, mode, out);
            continue;
        }

        // PrefixSequenceOnly:
        // - no recursion into params
        // - if current segment has params, ignore all following chain segments
        if (hasParams) { return; }
    }
}


void NameTreeStore::walkParams(
    const rapidjson::Value& params,
    NameTree*               parent,
    uint32_t                event,
    VisitMode               mode,
    std::vector<StrId>&     out) {
    if (mode == VisitMode::PrefixSequenceOnly) { return; }

    const auto args = params.GetArray();

    if (mode == VisitMode::Insert) {
        while (parent->indexed.size() < args.Size()) { parent->indexed.emplace_back(); }
    }

    for (rapidjson::SizeType k = 0; k < args.Size(); ++k) {
        const auto& arg = args[k];

        if (std::string_view{arg["name"]["name"].GetString()} == "<qualified>") {
            BranchingMap* indexedLevel = nullptr;
            if (mode == VisitMode::Insert) { indexedLevel = &parent->indexed[k]; }
            walkChainInto(arg["params"], indexedLevel, event, mode, out);
            continue;
        }

        const StrId name = interner_.intern(arg["name"]["name"].GetString());
        out.push_back(name);

        if (mode == VisitMode::Insert) {
            NameTree* node = nestedInsert(&parent->indexed[k], name);
            sets_.add(node->set, event);
            walkParams(arg["params"], node, event, mode, out);
        } else {
            walkParams(arg["params"], nullptr, event, mode, out);
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

SubsetCollection::BitmapView NameTreeStore::intersectAll(
    const std::vector<SetId>& constraints) {
    if (constraints.empty()) { return sets_.values(emptySet()); }
    SubsetCollection::BitmapPtr acc = SubsetCollection::makeBitmap();
    roaring_bitmap_overwrite(acc.get(), sets_.values(constraints.front()).raw());
    for (std::size_t i = 1; i < constraints.size(); ++i) {
        roaring_bitmap_and_inplace(acc.get(), sets_.values(constraints[i]).raw());
    }
    return SubsetCollection::BitmapView{acc};
}
