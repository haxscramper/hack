#pragma once

#include <roaring/roaring.h>

#include <cstddef>
#include <cstdint>
#include <memory>
#include <unordered_map>
#include <vector>

#include "strong_id.hpp"

using SetId = StrongId<struct SetIdTag>;

/// \class SubsetCollection
/// \brief Stores multiple integer sets and supports direct and
/// inverted-index queries.
class SubsetCollection {
  public:
    /// \brief Shared owning handle for a Roaring bitmap.
    using BitmapPtr = std::shared_ptr<roaring_bitmap_t>;

    /// \brief Creates an empty bitmap with the proper deleter.
    /// \return Shared bitmap handle.
    static BitmapPtr makeBitmap();

    /// \brief Converts a set id to a vector index.
    /// \param id Logical set id.
    /// \return Index in internal storage.
    static std::size_t toIndex(SetId id) noexcept;

    /// \class BitmapView
    /// \brief Lightweight shared view over a bitmap.
    class BitmapView {
      public:
        /// \brief Constructs a view from a shared bitmap handle.
        /// \param bm Shared bitmap pointer.
        explicit BitmapView(BitmapPtr bm);

        /// \brief Returns number of values in the bitmap.
        /// \return Cardinality.
        uint64_t cardinality() const;

        /// \brief Checks whether a value is present.
        /// \param value Value to test.
        /// \return True if present.
        bool contains(uint32_t value) const;

        /// \brief Iterates all values in ascending order.
        /// \tparam Fn Callable accepting `uint32_t`.
        /// \param fn Callback invoked for each value.
        template <typename Fn>
        void forEach(Fn&& fn) const {
            roaring_uint32_iterator_t it;
            roaring_iterator_init(bm_.get(), &it);
            while (it.has_value) {
                fn(it.current_value);
                roaring_uint32_iterator_advance(&it);
            }
        }

        /// \brief Materializes bitmap values into a vector.
        /// \return Sorted vector of values.
        std::vector<uint32_t> toVector() const;

        /// \brief Returns raw pointer for read-only interop.
        /// \return Raw bitmap pointer.
        const roaring_bitmap_t* raw() const;

      private:
        BitmapPtr bm_;
    };

    SubsetCollection() = default;

    SubsetCollection(const SubsetCollection&)            = delete;
    SubsetCollection& operator=(const SubsetCollection&) = delete;

    SubsetCollection(SubsetCollection&&) noexcept            = default;
    SubsetCollection& operator=(SubsetCollection&&) noexcept = default;

    /// \brief Creates a new empty set.
    /// \return New set id.
    SetId createSet();

    /// \brief Adds one value to a set and updates the inverted index.
    /// \param id Target set id.
    /// \param value Value to insert.
    void add(SetId id, uint32_t value);

    /// \brief Adds many values to a set and updates the inverted index.
    /// \param id Target set id.
    /// \param n Number of values.
    /// \param values Pointer to values array.
    void addMany(SetId id, std::size_t n, const uint32_t* values);

    /// \brief Applies run optimization and shrink-to-fit to one set.
    /// \param id Target set id.
    void optimize(SetId id);

    /// \brief Applies run optimization and shrink-to-fit to all bitmaps.
    void optimizeAll();

    /// \brief Returns cardinality of a set.
    /// \param id Set id.
    /// \return Number of values.
    uint64_t cardinality(SetId id) const;

    /// \brief Checks whether a set contains a value.
    /// \param id Set id.
    /// \param value Value to test.
    /// \return True if present.
    bool contains(SetId id, uint32_t value) const;

    /// \brief Returns cardinality of intersection of two sets.
    /// \param a First set id.
    /// \param b Second set id.
    /// \return Intersection cardinality.
    uint64_t intersectionCardinality(SetId a, SetId b) const;

    /// \brief Builds intersection bitmap of two sets.
    /// \param a First set id.
    /// \param b Second set id.
    /// \return View over a new bitmap containing intersection.
    BitmapView intersection(SetId a, SetId b) const;

    /// \brief Returns a shared view of set values.
    /// \param id Set id.
    /// \return View over stored bitmap.
    BitmapView values(SetId id) const;

    /// \brief Finds set ids that contain all given values.
    /// \param values Values that must all be present.
    /// \return View over matching set ids.
    BitmapView setsContainingAll(
        const std::vector<uint32_t>& values) const;

    /// \brief Finds set ids that contain any of the given values.
    /// \param values Values where at least one must be present.
    /// \return View over matching set ids.
    BitmapView setsContainingAny(
        const std::vector<uint32_t>& values) const;

    /// \brief Returns number of created sets.
    /// \return Set count.
    std::size_t setCount() const;

  private:
    /// \brief Returns validated set bitmap by id.
    /// \param id Set id.
    /// \return Reference to shared bitmap pointer.
    /// \throws std::out_of_range If id is invalid.
    const BitmapPtr& at(SetId id) const;

    /// \brief Returns posting list for a value, creating it if absent.
    /// \param value Value key.
    /// \return Mutable posting list handle.
    BitmapPtr& invertedFor(uint32_t value);

    /// \brief Returns posting list for a value if present.
    /// \param value Value key.
    /// \return Raw posting list pointer or null.
    const roaring_bitmap_t* findInverted(uint32_t value) const;

    std::vector<BitmapPtr>                  sets;
    std::unordered_map<uint32_t, BitmapPtr> inverted;
};
