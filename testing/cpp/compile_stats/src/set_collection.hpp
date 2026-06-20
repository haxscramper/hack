// set_collection.hpp
#pragma once

#include <cudd.h>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <unordered_map>
#include <vector>

#include "strong_id.hpp"

using SetId   = StrongId<struct SetIdTag>;
using CanonId = StrongId<struct CanonIdTag>;

template <>
struct std::hash<SetId> {
    std::size_t operator()(SetId id) const noexcept {
        return std::hash<uint32_t>{}(id.raw());
    }
};

template <>
struct std::hash<CanonId> {
    std::size_t operator()(CanonId id) const noexcept {
        return std::hash<uint32_t>{}(id.raw());
    }
};

/// \class SubsetCollection
/// \brief Stores many integer sets with batched compaction via CUDD ZDD.
///
/// Mutations are staged. Calling `optimize()` / `optimizeAll()` rebuilds
/// canonical ZDD-backed storage and posting indexes in one batch.
class SubsetCollection {
  public:
    /// \class View
    /// \brief Lightweight shared view over sorted uint32 ids/values.
    class View {
      public:
        explicit View(std::shared_ptr<const std::vector<uint32_t>> data);

        /// \brief Number of elements.
        uint64_t cardinality() const;

        /// \brief Checks membership.
        bool contains(uint32_t value) const;

        /// \brief Iterates values in ascending order.
        template <typename Fn>
        void forEach(Fn&& fn) const {
            for (uint32_t v : *data_) { fn(v); }
        }

        /// \brief Materializes values.
        std::vector<uint32_t> toVector() const;

      private:
        std::shared_ptr<const std::vector<uint32_t>> data_;
    };

    SubsetCollection();
    ~SubsetCollection();

    SubsetCollection(const SubsetCollection&)            = delete;
    SubsetCollection& operator=(const SubsetCollection&) = delete;

    SubsetCollection(SubsetCollection&&)            = delete;
    SubsetCollection& operator=(SubsetCollection&&) = delete;

    /// \brief Creates a new empty set.
    SetId createSet();

    /// \brief Adds one value to a set (staged).
    void add(SetId id, uint32_t value);

    /// \brief Adds many values to a set (staged).
    void addMany(SetId id, std::size_t n, const uint32_t* values);

    /// \brief Rebuilds compact canonical storage in batch.
    ///
    /// The argument is accepted for API compatibility; compaction is global.
    void optimize(SetId id);

    /// \brief Rebuilds compact canonical storage in batch.
    void optimizeAll();

    /// \brief Cardinality of a set.
    uint64_t cardinality(SetId id) const;

    /// \brief Membership test.
    bool contains(SetId id, uint32_t value) const;

    /// \brief Cardinality of pairwise set intersection.
    uint64_t intersectionCardinality(SetId a, SetId b) const;

    /// \brief Pairwise set intersection.
    View intersection(SetId a, SetId b) const;

    /// \brief Intersects values of all provided sets in backend-native form.
    /// \param ids Set ids to intersect.
    /// \return View over the intersection result.
    View intersection(const std::vector<SetId>& ids) const;

    /// \brief Values of one set.
    View values(SetId id) const;

    /// \brief Set IDs containing all requested values.
    View setsContainingAll(const std::vector<uint32_t>& values) const;

    /// \brief Set IDs containing any requested value.
    View setsContainingAny(const std::vector<uint32_t>& values) const;

    /// \brief Number of created sets.
    std::size_t setCount() const;

  private:
    struct CuddDeleter {
        void operator()(DdManager* m) const noexcept {
            if (m) { Cudd_Quit(m); }
        }
    };

    static std::size_t toIndex(SetId id) noexcept {
        return static_cast<std::size_t>(id.raw());
    }

    void                         requireValid(SetId id) const;
    void                         ensureMutable(SetId id);
    const std::vector<uint32_t>& currentValues(std::size_t setIdx) const;
    static void insertSortedUnique(std::vector<uint32_t>& vec, uint32_t v);

    void rebuildCanonical();
    void clearCanonical();

    void    ensureZddVar(uint32_t idx);
    DdNode* buildSingletonSetZdd(const std::vector<uint32_t>& sortedUniqueValues);

  private:
    std::unique_ptr<DdManager, CuddDeleter> mgr_;

    // Staged mutable data (used between optimize calls).
    std::vector<std::vector<uint32_t>> staged_sets_;
    std::vector<uint8_t>               staged_dirty_set_; // 0/1
    bool                               postings_dirty_ = true;

    // Canonical compact data (valid after optimize).
    std::vector<DdNode*>                                      canon_nodes_;
    std::vector<std::shared_ptr<const std::vector<uint32_t>>> canon_values_;
    std::vector<CanonId>                                      set_to_canon_;
    std::vector<std::vector<uint32_t>>                        canon_to_sets_;
    std::vector<std::vector<uint32_t>>                        value_to_canon_;
};
