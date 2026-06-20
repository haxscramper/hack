#include "set_collection.hpp"

#include <gtest/gtest.h>
#include <algorithm>
#include <vector>

namespace {

std::vector<uint32_t> sortedVector(
    const SubsetCollection::BitmapView& view) {
    std::vector<uint32_t> v = view.toVector();
    std::sort(v.begin(), v.end());
    return v;
}

} // namespace

TEST(SubsetCollection, CreateAndAdd) {
    SubsetCollection c;
    auto             id = c.createSet();
    EXPECT_EQ(id, 0u);
    c.add(id, 5);
    c.add(id, 10);
    c.add(id, 5); // duplicate is a no-op
    EXPECT_EQ(c.cardinality(id), 2u);
    EXPECT_TRUE(c.contains(id, 5));
    EXPECT_TRUE(c.contains(id, 10));
    EXPECT_FALSE(c.contains(id, 7));
}

TEST(SubsetCollection, MultipleSetIds) {
    SubsetCollection c;
    auto             a = c.createSet();
    auto             b = c.createSet();
    EXPECT_EQ(a, 0u);
    EXPECT_EQ(b, 1u);
    EXPECT_EQ(c.setCount(), 2u);
}

TEST(SubsetCollection, AddMany) {
    SubsetCollection      c;
    auto                  id   = c.createSet();
    std::vector<uint32_t> vals = {1, 2, 3, 100, 200};
    c.addMany(id, vals.size(), vals.data());
    EXPECT_EQ(c.cardinality(id), 5u);
    EXPECT_EQ(sortedVector(c.values(id)), vals);
}

TEST(SubsetCollection, IntersectionCardinality) {
    SubsetCollection c;
    auto             a = c.createSet();
    auto             b = c.createSet();
    for (uint32_t v : {1u, 2u, 3u, 4u}) { c.add(a, v); }
    for (uint32_t v : {3u, 4u, 5u, 6u}) { c.add(b, v); }
    EXPECT_EQ(c.intersectionCardinality(a, b), 2u);
}

TEST(SubsetCollection, IntersectionValues) {
    SubsetCollection c;
    auto             a = c.createSet();
    auto             b = c.createSet();
    for (uint32_t v : {1u, 2u, 3u, 4u}) { c.add(a, v); }
    for (uint32_t v : {3u, 4u, 5u, 6u}) { c.add(b, v); }
    std::vector<uint32_t> expected = {3u, 4u};
    EXPECT_EQ(sortedVector(c.intersection(a, b)), expected);
}

TEST(SubsetCollection, ValuesViewSharesData) {
    SubsetCollection c;
    auto             id = c.createSet();
    c.add(id, 42);
    auto view = c.values(id);
    EXPECT_TRUE(view.contains(42));
    EXPECT_EQ(view.cardinality(), 1u);
}

TEST(SubsetCollection, SetsContainingAll) {
    SubsetCollection c;
    auto             s0 = c.createSet();
    auto             s1 = c.createSet();
    auto             s2 = c.createSet();
    for (uint32_t v : {1u, 2u, 3u}) { c.add(s0, v); }
    for (uint32_t v : {2u, 3u, 4u}) { c.add(s1, v); }
    for (uint32_t v : {3u, 4u, 5u}) { c.add(s2, v); }

    // Sets containing both 2 and 3 -> s0, s1
    auto                  r = sortedVector(c.setsContainingAll({2u, 3u}));
    std::vector<uint32_t> expected = {s0, s1};
    EXPECT_EQ(r, expected);

    // Sets containing 3 -> all
    auto                  r3  = sortedVector(c.setsContainingAll({3u}));
    std::vector<uint32_t> all = {s0, s1, s2};
    EXPECT_EQ(r3, all);

    // Value present nowhere
    EXPECT_EQ(c.setsContainingAll({999u}).cardinality(), 0u);

    // Empty query
    EXPECT_EQ(c.setsContainingAll({}).cardinality(), 0u);
}

TEST(SubsetCollection, SetsContainingAny) {
    SubsetCollection c;
    auto             s0 = c.createSet();
    auto             s1 = c.createSet();
    auto             s2 = c.createSet();
    for (uint32_t v : {1u, 2u}) { c.add(s0, v); }
    for (uint32_t v : {3u, 4u}) { c.add(s1, v); }
    for (uint32_t v : {5u, 6u}) { c.add(s2, v); }

    // Any of {1, 5} -> s0, s2
    auto                  r = sortedVector(c.setsContainingAny({1u, 5u}));
    std::vector<uint32_t> expected = {s0, s2};
    EXPECT_EQ(r, expected);

    auto rNone = c.setsContainingAny({999u});
    EXPECT_EQ(rNone.cardinality(), 0u);
}

TEST(SubsetCollection, ForEachIteration) {
    SubsetCollection      c;
    auto                  id   = c.createSet();
    std::vector<uint32_t> vals = {10, 20, 30, 40};
    c.addMany(id, vals.size(), vals.data());

    std::vector<uint32_t> seen;
    c.values(id).forEach([&](uint32_t v) { seen.push_back(v); });
    std::sort(seen.begin(), seen.end());
    EXPECT_EQ(seen, vals);
}

TEST(SubsetCollection, OptimizePreservesContents) {
    SubsetCollection c;
    auto             id = c.createSet();
    for (uint32_t v = 0; v < 1000; ++v) {
        c.add(id, v); // contiguous -> run container
    }
    c.optimizeAll();
    EXPECT_EQ(c.cardinality(id), 1000u);
    EXPECT_TRUE(c.contains(id, 0));
    EXPECT_TRUE(c.contains(id, 999));
    EXPECT_FALSE(c.contains(id, 1000));
}

TEST(SubsetCollection, InvalidSetIdThrows) {
    SubsetCollection c;
    EXPECT_THROW(c.cardinality(0), std::out_of_range);
    auto id = c.createSet();
    EXPECT_NO_THROW(c.cardinality(id));
    EXPECT_THROW(c.cardinality(id + 1), std::out_of_range);
}
