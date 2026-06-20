// set_collection.cpp
#include "set_collection.hpp"

#include <stdexcept>
#include <utility>

SubsetCollection::View::View(std::shared_ptr<const std::vector<uint32_t>> data)
    : data_(std::move(data)) {}

uint64_t SubsetCollection::View::cardinality() const { return data_ ? data_->size() : 0; }

bool SubsetCollection::View::contains(uint32_t value) const {
    if (!data_) { return false; }
    return std::binary_search(data_->begin(), data_->end(), value);
}

std::vector<uint32_t> SubsetCollection::View::toVector() const {
    if (!data_) { return {}; }
    return *data_;
}

SubsetCollection::SubsetCollection()
    : mgr_(Cudd_Init(0, 0, CUDD_UNIQUE_SLOTS, CUDD_CACHE_SLOTS, 0)) {
    if (!mgr_) { throw std::runtime_error("Cudd_Init failed"); }
    // Keep reordering off here. It was triggering GC consistency checks
    // while we are still validating ref discipline.
    Cudd_AutodynDisableZdd(mgr_.get());
}


SubsetCollection::~SubsetCollection() { clearCanonical(); }

SetId SubsetCollection::createSet() {
    staged_sets_.push_back({});
    staged_dirty_set_.push_back(1);
    postings_dirty_ = true;
    return static_cast<SetId>(staged_sets_.size() - 1);
}

void SubsetCollection::requireValid(SetId id) const {
    if (toIndex(id) >= staged_sets_.size()) { throw std::out_of_range("invalid SetId"); }
}

void SubsetCollection::insertSortedUnique(std::vector<uint32_t>& vec, uint32_t v) {
    auto it = std::lower_bound(vec.begin(), vec.end(), v);
    if (it == vec.end() || *it != v) { vec.insert(it, v); }
}

void SubsetCollection::ensureMutable(SetId id) {
    requireValid(id);
    const std::size_t idx = toIndex(id);
    if (staged_dirty_set_[idx]) { return; }

    staged_sets_[idx]      = *canon_values_.at(set_to_canon_.at(idx).raw());
    staged_dirty_set_[idx] = 1;
}

const std::vector<uint32_t>& SubsetCollection::currentValues(std::size_t setIdx) const {
    if (staged_dirty_set_[setIdx]) { return staged_sets_[setIdx]; }
    return *canon_values_.at(set_to_canon_.at(setIdx).raw());
}

void SubsetCollection::add(SetId id, uint32_t value) {
    ensureMutable(id);
    insertSortedUnique(staged_sets_[toIndex(id)], value);
    postings_dirty_ = true;
}

void SubsetCollection::addMany(SetId id, std::size_t n, const uint32_t* values) {
    ensureMutable(id);
    auto& dst = staged_sets_[toIndex(id)];
    for (std::size_t i = 0; i < n; ++i) { insertSortedUnique(dst, values[i]); }
    postings_dirty_ = true;
}

void SubsetCollection::ensureZddVar(uint32_t idx) {
    while (static_cast<uint32_t>(Cudd_ReadZddSize(mgr_.get())) <= idx) {
        const int varIndex = Cudd_ReadZddSize(mgr_.get());
        DdNode*   var      = Cudd_zddIthVar(mgr_.get(), varIndex);
        if (!var) { throw std::runtime_error("Cudd_zddIthVar failed"); }
    }
}


DdNode* SubsetCollection::buildSingletonSetZdd(
    const std::vector<uint32_t>& sortedUniqueValues) {
    if (!sortedUniqueValues.empty()) { ensureZddVar(sortedUniqueValues.back()); }

    DdNode* node = Cudd_ReadZddOne(mgr_.get(), 0);
    Cudd_Ref(node);

    for (uint32_t v : sortedUniqueValues) {
        DdNode* next = Cudd_zddChange(mgr_.get(), node, static_cast<int>(v));
        if (!next) {
            Cudd_RecursiveDerefZdd(mgr_.get(), node);
            throw std::runtime_error("Cudd_zddChange failed");
        }
        Cudd_Ref(next);
        Cudd_RecursiveDerefZdd(mgr_.get(), node);
        node = next;
    }

    return node; // referenced
}


void SubsetCollection::clearCanonical() {
    for (DdNode* n : canon_nodes_) { Cudd_RecursiveDerefZdd(mgr_.get(), n); }
    canon_nodes_.clear();
    canon_values_.clear();
    set_to_canon_.clear();
    canon_to_sets_.clear();
    value_to_canon_.clear();
}

void SubsetCollection::rebuildCanonical() {
    clearCanonical();

    set_to_canon_.resize(staged_sets_.size());
    std::unordered_map<DdNode*, uint32_t> node_to_canon;

    for (std::size_t setIdx = 0; setIdx < staged_sets_.size(); ++setIdx) {
        std::vector<uint32_t> vals = currentValues(setIdx);
        std::sort(vals.begin(), vals.end());
        vals.erase(std::unique(vals.begin(), vals.end()), vals.end());

        DdNode* node = buildSingletonSetZdd(vals); // returns Cudd_Ref'ed

        auto [it, inserted] = node_to_canon.emplace(
            node, static_cast<uint32_t>(canon_nodes_.size()));

        uint32_t cid = 0;
        if (inserted) {
            cid = static_cast<uint32_t>(canon_nodes_.size());
            canon_nodes_.push_back(node); // keep this ref until clearCanonical()
            canon_values_.push_back(
                std::make_shared<const std::vector<uint32_t>>(std::move(vals)));
            canon_to_sets_.push_back({});
        } else {
            cid = it->second;
            Cudd_RecursiveDerefZdd(mgr_.get(), node); // drop duplicate ref
        }

        set_to_canon_[setIdx] = static_cast<CanonId>(cid);
        canon_to_sets_[cid].push_back(static_cast<uint32_t>(setIdx));
    }

    uint32_t maxV   = 0;
    bool     hasAny = false;
    for (const auto& vals : canon_values_) {
        if (!vals->empty()) {
            hasAny = true;
            maxV   = std::max(maxV, vals->back());
        }
    }

    value_to_canon_.clear();
    if (hasAny) {
        value_to_canon_.resize(static_cast<std::size_t>(maxV) + 1);
        for (uint32_t cid = 0; cid < canon_values_.size(); ++cid) {
            for (uint32_t v : *canon_values_[cid]) { value_to_canon_[v].push_back(cid); }
        }
        for (auto& posting : value_to_canon_) {
            std::sort(posting.begin(), posting.end());
            posting.erase(std::unique(posting.begin(), posting.end()), posting.end());
        }
    }

    for (std::size_t i = 0; i < staged_sets_.size(); ++i) {
        staged_sets_[i].clear();
        staged_sets_[i].shrink_to_fit();
        staged_dirty_set_[i] = 0;
    }

    postings_dirty_ = false;

    // Intentionally no Cudd_zddReduceHeap here.
}


void SubsetCollection::optimize(SetId) { rebuildCanonical(); }

void SubsetCollection::optimizeAll() { rebuildCanonical(); }

uint64_t SubsetCollection::cardinality(SetId id) const {
    requireValid(id);
    return currentValues(toIndex(id)).size();
}

bool SubsetCollection::contains(SetId id, uint32_t value) const {
    requireValid(id);
    const auto& vals = currentValues(toIndex(id));
    return std::binary_search(vals.begin(), vals.end(), value);
}

uint64_t SubsetCollection::intersectionCardinality(SetId a, SetId b) const {
    requireValid(a);
    requireValid(b);

    const auto& va = currentValues(toIndex(a));
    const auto& vb = currentValues(toIndex(b));

    std::size_t i = 0;
    std::size_t j = 0;
    uint64_t    c = 0;

    while (i < va.size() && j < vb.size()) {
        if (va[i] == vb[j]) {
            ++c;
            ++i;
            ++j;
        } else if (va[i] < vb[j]) {
            ++i;
        } else {
            ++j;
        }
    }

    return c;
}

SubsetCollection::View SubsetCollection::intersection(SetId a, SetId b) const {
    requireValid(a);
    requireValid(b);

    const auto& va = currentValues(toIndex(a));
    const auto& vb = currentValues(toIndex(b));

    auto out = std::make_shared<std::vector<uint32_t>>();
    out->reserve(std::min(va.size(), vb.size()));

    std::set_intersection(
        va.begin(), va.end(), vb.begin(), vb.end(), std::back_inserter(*out));

    return View(std::move(out));
}

SubsetCollection::View SubsetCollection::intersection(
    const std::vector<SetId>& ids) const {
    if (ids.empty()) { return View(std::make_shared<const std::vector<uint32_t>>()); }

    if (ids.size() == 1) { return values(ids.front()); }

    std::vector<const std::vector<uint32_t>*> inputs;
    inputs.reserve(ids.size());

    for (SetId id : ids) {
        requireValid(id);
        const auto& vals = currentValues(toIndex(id));
        if (vals.empty()) {
            return View(std::make_shared<const std::vector<uint32_t>>());
        }
        inputs.push_back(&vals);
    }

    std::sort(
        inputs.begin(),
        inputs.end(),
        [](const std::vector<uint32_t>* a, const std::vector<uint32_t>* b) {
            return a->size() < b->size();
        });

    std::vector<uint32_t> acc = *inputs.front();
    std::vector<uint32_t> tmp;

    for (std::size_t i = 1; i < inputs.size() && !acc.empty(); ++i) {
        const auto& next = *inputs[i];
        tmp.clear();
        tmp.reserve(std::min(acc.size(), next.size()));

        std::set_intersection(
            acc.begin(), acc.end(), next.begin(), next.end(), std::back_inserter(tmp));

        acc.swap(tmp);
    }

    return View(std::make_shared<const std::vector<uint32_t>>(std::move(acc)));
}


SubsetCollection::View SubsetCollection::values(SetId id) const {
    requireValid(id);
    const auto& vals = currentValues(toIndex(id));
    return View(std::make_shared<const std::vector<uint32_t>>(vals));
}

SubsetCollection::View SubsetCollection::setsContainingAll(
    const std::vector<uint32_t>& values) const {
    if (values.empty()) { return View(std::make_shared<const std::vector<uint32_t>>()); }

    auto out = std::make_shared<std::vector<uint32_t>>();

    if (!postings_dirty_) {
        std::vector<uint32_t> canon_acc;
        bool                  initialized = false;

        for (uint32_t v : values) {
            if (v >= value_to_canon_.size()) {
                return View(std::make_shared<const std::vector<uint32_t>>());
            }

            const auto& posting = value_to_canon_[v];
            if (posting.empty()) {
                return View(std::make_shared<const std::vector<uint32_t>>());
            }

            if (!initialized) {
                canon_acc   = posting;
                initialized = true;
            } else {
                std::vector<uint32_t> next;
                next.reserve(std::min(canon_acc.size(), posting.size()));
                std::set_intersection(
                    canon_acc.begin(),
                    canon_acc.end(),
                    posting.begin(),
                    posting.end(),
                    std::back_inserter(next));
                canon_acc.swap(next);
                if (canon_acc.empty()) {
                    return View(std::make_shared<const std::vector<uint32_t>>());
                }
            }
        }

        for (uint32_t cid : canon_acc) {
            const auto& setIds = canon_to_sets_[cid];
            out->insert(out->end(), setIds.begin(), setIds.end());
        }
        std::sort(out->begin(), out->end());
        return View(std::move(out));
    }

    // Slow path while staged changes exist (before optimize).
    for (uint32_t sid = 0; sid < staged_sets_.size(); ++sid) {
        const auto& vals = currentValues(sid);
        bool        ok   = true;
        for (uint32_t v : values) {
            if (!std::binary_search(vals.begin(), vals.end(), v)) {
                ok = false;
                break;
            }
        }
        if (ok) { out->push_back(sid); }
    }

    return View(std::move(out));
}

SubsetCollection::View SubsetCollection::setsContainingAny(
    const std::vector<uint32_t>& values) const {
    auto out = std::make_shared<std::vector<uint32_t>>();

    if (!postings_dirty_) {
        std::vector<uint8_t> canon_seen(canon_values_.size(), 0);

        for (uint32_t v : values) {
            if (v >= value_to_canon_.size()) { continue; }
            for (uint32_t cid : value_to_canon_[v]) { canon_seen[cid] = 1; }
        }

        for (uint32_t cid = 0; cid < canon_seen.size(); ++cid) {
            if (!canon_seen[cid]) { continue; }
            const auto& setIds = canon_to_sets_[cid];
            out->insert(out->end(), setIds.begin(), setIds.end());
        }

        std::sort(out->begin(), out->end());
        return View(std::move(out));
    }

    // Slow path while staged changes exist (before optimize).
    for (uint32_t sid = 0; sid < staged_sets_.size(); ++sid) {
        const auto& vals = currentValues(sid);
        bool        ok   = false;
        for (uint32_t v : values) {
            if (std::binary_search(vals.begin(), vals.end(), v)) {
                ok = true;
                break;
            }
        }
        if (ok) { out->push_back(sid); }
    }

    return View(std::move(out));
}

std::size_t SubsetCollection::setCount() const { return staged_sets_.size(); }
