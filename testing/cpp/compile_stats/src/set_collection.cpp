#include "set_collection.hpp"

#include <stdexcept>
#include <utility>

SubsetCollection::BitmapPtr SubsetCollection::makeBitmap() {
    return BitmapPtr(roaring_bitmap_create(), roaring_bitmap_free);
}

std::size_t SubsetCollection::toIndex(SetId id) noexcept {
    return static_cast<std::size_t>(id.raw());
}

SubsetCollection::BitmapView::BitmapView(BitmapPtr bm)
    : bm_(std::move(bm)) {}

uint64_t SubsetCollection::BitmapView::cardinality() const {
    return roaring_bitmap_get_cardinality(bm_.get());
}

bool SubsetCollection::BitmapView::contains(uint32_t value) const {
    return roaring_bitmap_contains(bm_.get(), value);
}

std::vector<uint32_t> SubsetCollection::BitmapView::toVector() const {
    std::vector<uint32_t> out;
    out.resize(roaring_bitmap_get_cardinality(bm_.get()));
    roaring_bitmap_to_uint32_array(bm_.get(), out.data());
    return out;
}

const roaring_bitmap_t* SubsetCollection::BitmapView::raw() const {
    return bm_.get();
}

SetId SubsetCollection::createSet() {
    sets.push_back(makeBitmap());
    return static_cast<SetId>(sets.size() - 1);
}

void SubsetCollection::add(SetId id, uint32_t value) {
    roaring_bitmap_add(at(id).get(), value);
    roaring_bitmap_add(invertedFor(value).get(), id.raw());
}

void SubsetCollection::addMany(
    SetId           id,
    std::size_t     n,
    const uint32_t* values) {
    roaring_bitmap_t* bm = at(id).get();
    roaring_bitmap_add_many(bm, n, values);

    for (std::size_t i = 0; i < n; ++i) {
        roaring_bitmap_add(invertedFor(values[i]).get(), id.raw());
    }
}

void SubsetCollection::optimize(SetId id) {
    roaring_bitmap_t* bm = at(id).get();
    roaring_bitmap_run_optimize(bm);
    roaring_bitmap_shrink_to_fit(bm);
}

void SubsetCollection::optimizeAll() {
    for (auto& bm : sets) {
        roaring_bitmap_run_optimize(bm.get());
        roaring_bitmap_shrink_to_fit(bm.get());
    }

    for (auto& kv : inverted) {
        roaring_bitmap_run_optimize(kv.second.get());
        roaring_bitmap_shrink_to_fit(kv.second.get());
    }
}

uint64_t SubsetCollection::cardinality(SetId id) const {
    return roaring_bitmap_get_cardinality(at(id).get());
}

bool SubsetCollection::contains(SetId id, uint32_t value) const {
    return roaring_bitmap_contains(at(id).get(), value);
}

uint64_t SubsetCollection::intersectionCardinality(SetId a, SetId b)
    const {
    return roaring_bitmap_and_cardinality(at(a).get(), at(b).get());
}

SubsetCollection::BitmapView SubsetCollection::intersection(
    SetId a,
    SetId b) const {
    BitmapPtr r(
        roaring_bitmap_and(at(a).get(), at(b).get()), roaring_bitmap_free);
    return BitmapView(std::move(r));
}

SubsetCollection::BitmapView SubsetCollection::values(SetId id) const {
    return BitmapView(sets.at(id.raw()));
}

SubsetCollection::BitmapView SubsetCollection::setsContainingAll(
    const std::vector<uint32_t>& values) const {
    if (values.empty()) { return BitmapView(makeBitmap()); }

    const roaring_bitmap_t* first = findInverted(values[0]);
    if (!first) { return BitmapView(makeBitmap()); }

    BitmapPtr acc(roaring_bitmap_copy(first), roaring_bitmap_free);

    for (std::size_t i = 1; i < values.size(); ++i) {
        const roaring_bitmap_t* posting = findInverted(values[i]);
        if (!posting) { return BitmapView(makeBitmap()); }

        roaring_bitmap_and_inplace(acc.get(), posting);

        if (roaring_bitmap_is_empty(acc.get())) { break; }
    }

    return BitmapView(std::move(acc));
}

SubsetCollection::BitmapView SubsetCollection::setsContainingAny(
    const std::vector<uint32_t>& values) const {
    BitmapPtr acc = makeBitmap();

    for (uint32_t v : values) {
        const roaring_bitmap_t* posting = findInverted(v);
        if (posting) { roaring_bitmap_or_inplace(acc.get(), posting); }
    }

    return BitmapView(std::move(acc));
}

std::size_t SubsetCollection::setCount() const { return sets.size(); }

const SubsetCollection::BitmapPtr& SubsetCollection::at(SetId id) const {
    if (toIndex(id) >= sets.size() || !sets[id.raw()]) {
        throw std::out_of_range("invalid SetId");
    }

    return sets[toIndex(id)];
}

SubsetCollection::BitmapPtr& SubsetCollection::invertedFor(
    uint32_t value) {
    auto it = inverted.find(value);
    if (it == inverted.end()) {
        it = inverted.emplace(value, makeBitmap()).first;
    }

    return it->second;
}

const roaring_bitmap_t* SubsetCollection::findInverted(
    uint32_t value) const {
    auto it = inverted.find(value);
    if (it == inverted.end()) { return nullptr; }

    return it->second.get();
}
