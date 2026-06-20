#include <roaring/roaring.h>
#include <cstdint>
#include <vector>
#include <memory>
#include <unordered_map>
#include <stdexcept>

#include <boost/serialization/strong_typedef.hpp>
#include "strong_id.hpp"

using SetId = StrongId<struct SetIdTag>;


class SubsetCollection {
  public:
    // Shared handle to a roaring bitmap with the proper deleter.
    using BitmapPtr = std::shared_ptr<roaring_bitmap_t>;

    static BitmapPtr makeBitmap() {
        return BitmapPtr(roaring_bitmap_create(), roaring_bitmap_free);
    }

    static inline std::size_t toIndex(SetId id) noexcept {
        return static_cast<std::size_t>(id.raw());
    }

    // Non-owning-copy view over a bitmap: holds only the shared_ptr.
    class BitmapView {
      public:
        explicit BitmapView(BitmapPtr bm) : bm_(std::move(bm)) {}

        uint64_t cardinality() const {
            return roaring_bitmap_get_cardinality(bm_.get());
        }

        bool contains(uint32_t value) const {
            return roaring_bitmap_contains(bm_.get(), value);
        }

        template <typename Fn>
        void forEach(Fn&& fn) const {
            roaring_uint32_iterator_t it;
            roaring_iterator_init(bm_.get(), &it);
            while (it.has_value) {
                fn(it.current_value);
                roaring_uint32_iterator_advance(&it);
            }
        }

        // Materialize on demand only if the caller wants it.
        std::vector<uint32_t> toVector() const {
            std::vector<uint32_t> out;
            out.resize(roaring_bitmap_get_cardinality(bm_.get()));
            roaring_bitmap_to_uint32_array(bm_.get(), out.data());
            return out;
        }

        const roaring_bitmap_t* raw() const { return bm_.get(); }

      private:
        BitmapPtr bm_;
    };

    SubsetCollection() = default;

    SubsetCollection(const SubsetCollection&)            = delete;
    SubsetCollection& operator=(const SubsetCollection&) = delete;

    SubsetCollection(SubsetCollection&&) noexcept            = default;
    SubsetCollection& operator=(SubsetCollection&&) noexcept = default;

    // --- Construction / population phase ---

    SetId createSet() {
        sets_.push_back(makeBitmap());
        return static_cast<SetId>(sets_.size() - 1);
    }

    void add(SetId id, uint32_t value) {
        roaring_bitmap_add(at(id).get(), value);
        roaring_bitmap_add(invertedFor(value).get(), id.raw()); // maintain
                                                          // inverted index
    }

    void addMany(SetId id, size_t n, const uint32_t* values) {
        roaring_bitmap_t* bm = at(id).get();
        roaring_bitmap_add_many(bm, n, values);
        for (size_t i = 0; i < n; ++i) {
            roaring_bitmap_add(invertedFor(values[i]).get(), id.raw());
        }
    }

    void optimize(SetId id) {
        roaring_bitmap_t* bm = at(id).get();
        roaring_bitmap_run_optimize(bm);
        roaring_bitmap_shrink_to_fit(bm);
    }

    void optimizeAll() {
        for (auto& bm : sets_) {
            roaring_bitmap_run_optimize(bm.get());
            roaring_bitmap_shrink_to_fit(bm.get());
        }
        for (auto& kv : inverted_) {
            roaring_bitmap_run_optimize(kv.second.get());
            roaring_bitmap_shrink_to_fit(kv.second.get());
        }
    }

    // --- Query phase ---

    uint64_t cardinality(SetId id) const {
        return roaring_bitmap_get_cardinality(at(id).get());
    }

    bool contains(SetId id, uint32_t value) const {
        return roaring_bitmap_contains(at(id).get(), value);
    }

    uint64_t intersectionCardinality(SetId a, SetId b) const {
        return roaring_bitmap_and_cardinality(at(a).get(), at(b).get());
    }

    BitmapView intersection(SetId a, SetId b) const {
        BitmapPtr r(
            roaring_bitmap_and(at(a).get(), at(b).get()),
            roaring_bitmap_free);
        return BitmapView(std::move(r));
    }

    BitmapView values(SetId id) const {
        return BitmapView(sets_.at(id.raw())); // shares the stored bitmap, no
                                         // copy
    }

    // (2) All set IDs whose sets contain *all* of the given values.
    BitmapView setsContainingAll(
        const std::vector<uint32_t>& values) const {
        if (values.empty()) { return BitmapView(makeBitmap()); }

        const roaring_bitmap_t* first = findInverted(values[0]);
        if (!first) {
            return BitmapView(makeBitmap()); // no set has values[0]
        }

        BitmapPtr acc(roaring_bitmap_copy(first), roaring_bitmap_free);
        for (size_t i = 1; i < values.size(); ++i) {
            const roaring_bitmap_t* posting = findInverted(values[i]);
            if (!posting) {
                return BitmapView(makeBitmap()); // some value present
                                                 // nowhere
            }
            roaring_bitmap_and_inplace(acc.get(), posting);
            if (roaring_bitmap_is_empty(acc.get())) { break; }
        }
        return BitmapView(std::move(acc));
    }

    // All set IDs whose sets contain *any* of the given values.
    BitmapView setsContainingAny(
        const std::vector<uint32_t>& values) const {
        BitmapPtr acc = makeBitmap();
        for (uint32_t v : values) {
            const roaring_bitmap_t* posting = findInverted(v);
            if (posting) { roaring_bitmap_or_inplace(acc.get(), posting); }
        }
        return BitmapView(std::move(acc));
    }

    size_t setCount() const { return sets_.size(); }

  private:
    const BitmapPtr& at(SetId id) const {
        if (toIndex(id) >= sets_.size() || !sets_[id.raw()]) {
            throw std::out_of_range("invalid SetId");
        }
        return sets_[toIndex(id)];
    }

    // Get (creating if needed) the inverted posting list for a value.
    BitmapPtr& invertedFor(uint32_t value) {
        auto it = inverted_.find(value);
        if (it == inverted_.end()) {
            it = inverted_.emplace(value, makeBitmap()).first;
        }
        return it->second;
    }

    const roaring_bitmap_t* findInverted(uint32_t value) const {
        auto it = inverted_.find(value);
        return it == inverted_.end() ? nullptr : it->second.get();
    }

    std::vector<BitmapPtr>                  sets_;
    std::unordered_map<uint32_t, BitmapPtr> inverted_;
};
