#include <gtest/gtest.h>
#include <rapidjson/document.h>

#include <algorithm>
#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/ranges.h>
#include <string>
#include <vector>

#include "name_tree.hpp"

namespace {

rapidjson::Document parse(const std::string& json) {
    rapidjson::Document doc;
    doc.Parse(json.c_str());
    EXPECT_FALSE(doc.HasParseError());
    return doc;
}

// std::vector<cpptrace::stacktrace_frame>::_M_move_assign
const char* kEv1 = R"({
  "names": {
    "name": {"name": "<qualified>", "cvr": []},
    "params": [
      {"name": {"name": "std", "cvr": []}, "params": []},
      {"name": {"name": "vector", "cvr": []}, "params": [
        {"name": {"name": "<qualified>", "cvr": []}, "params": [
          {"name": {"name": "cpptrace", "cvr": []}, "params": []},
          {"name": {"name": "stacktrace_frame", "cvr": []}, "params": []}
        ]}
      ]},
      {"name": {"name": "_M_move_assign", "cvr": []}, "params": []}
    ]
  }
})";

// std::vector<unsigned long>::push_back
const char* kEv2 = R"({
  "names": {
    "name": {"name": "<qualified>", "cvr": []},
    "params": [
      {"name": {"name": "std", "cvr": []}, "params": []},
      {"name": {"name": "vector", "cvr": []}, "params": [
        {"name": {"name": "long", "cvr": []}, "params": []}
      ]},
      {"name": {"name": "push_back", "cvr": []}, "params": []}
    ]
  }
})";

std::vector<uint32_t> toVec(SubsetCollection::BitmapView v) { return v.toVector(); }

NameTreeStore::NameTree* node(
    NameTreeStore&                  store,
    const std::vector<std::string>& chain) {
    std::vector<StrId> ids;
    for (const auto& s : chain) { ids.push_back(store.interner().intern(s)); }
    return store.findNamed(ids);
}

std::vector<uint32_t> setOf(NameTreeStore& store, const std::vector<std::string>& chain) {
    auto* n = node(store, chain);
    EXPECT_NE(n, nullptr);
    return store.sets().values(n->set).toVector();
}

} // namespace

TEST(NameTree, ChainAccumulatesEvents) {
    NameTreeStore store;
    store.insertEvent(EventId{1}, parse(kEv1));
    store.insertEvent(EventId{2}, parse(kEv2));

    EXPECT_EQ(setOf(store, {"std"}), (std::vector<uint32_t>{1, 2}));
    EXPECT_EQ(setOf(store, {"std", "vector"}), (std::vector<uint32_t>{1, 2}));
    EXPECT_EQ(setOf(store, {"std", "vector", "push_back"}), (std::vector<uint32_t>{2}));
    EXPECT_EQ(
        setOf(store, {"std", "vector", "_M_move_assign"}), (std::vector<uint32_t>{1}));
}

TEST(NameTree, IndexedArgEvents) {
    NameTreeStore store;
    store.insertEvent(EventId{1}, parse(kEv1));
    store.insertEvent(EventId{2}, parse(kEv2));

    auto* vec = node(store, {"std", "vector"});
    ASSERT_NE(vec, nullptr);
    ASSERT_EQ(vec->indexed.size(), 1u);

    const StrId cpptrace = store.interner().intern("cpptrace");
    const StrId lng      = store.interner().intern("long");

    auto cit = vec->indexed[0].find(cpptrace);
    auto lit = vec->indexed[0].find(lng);
    ASSERT_NE(cit, vec->indexed[0].end());
    ASSERT_NE(lit, vec->indexed[0].end());

    EXPECT_EQ(
        store.sets().values(cit->second->set).toVector(), (std::vector<uint32_t>{1}));
    EXPECT_EQ(
        store.sets().values(lit->second->set).toVector(), (std::vector<uint32_t>{2}));

    // Nested arg chain cpptrace::stacktrace_frame
    const StrId frame = store.interner().intern("stacktrace_frame");
    auto        fit   = cit->second->named.find(frame);
    ASSERT_NE(fit, cit->second->named.end());
    EXPECT_EQ(
        store.sets().values(fit->second->set).toVector(), (std::vector<uint32_t>{1}));
}

// Query: std::vector<...>::size-style -> any vector instantiation
TEST(NameTree, QueryAnyVector) {
    NameTreeStore store;
    store.insertEvent(EventId{1}, parse(kEv1));
    store.insertEvent(EventId{2}, parse(kEv2));

    const char* q = R"({
      "names": {"name": {"name": "<qualified>", "cvr": []}, "params": [
        {"name": {"name": "std", "cvr": []}, "params": []},
        {"name": {"name": "vector", "cvr": []}, "params": []}
      ]}
    })";

    EXPECT_EQ(toVec(store.query(parse(q))), (std::vector<uint32_t>{1, 2}));
}

// Query: std::vector<long> -> only EV2
TEST(NameTree, QueryConcreteArg) {
    NameTreeStore store;
    store.insertEvent(EventId{1}, parse(kEv1));
    store.insertEvent(EventId{2}, parse(kEv2));

    const char* q = R"({
      "names": {"name": {"name": "<qualified>", "cvr": []}, "params": [
        {"name": {"name": "std", "cvr": []}, "params": []},
        {"name": {"name": "vector", "cvr": []}, "params": [
          {"name": {"name": "long", "cvr": []}, "params": []}
        ]}
      ]}
    })";

    EXPECT_EQ(toVec(store.query(parse(q))), (std::vector<uint32_t>{2}));
}

// Query with wildcard arg: std::vector<?> -> both events
TEST(NameTree, QueryWildcardArg) {
    NameTreeStore store;
    store.insertEvent(EventId{1}, parse(kEv1));
    store.insertEvent(EventId{2}, parse(kEv2));

    const char* q = R"({
      "names": {"name": {"name": "<qualified>", "cvr": []}, "params": [
        {"name": {"name": "std", "cvr": []}, "params": []},
        {"name": {"name": "vector", "cvr": []}, "params": [
          {"name": {"name": "<query_wildcard>", "cvr": []}, "params": []}
        ]}
      ]}
    })";

    EXPECT_EQ(toVec(store.query(parse(q))), (std::vector<uint32_t>{1, 2}));
}

// Query miss: std::vector<int> -> no events
TEST(NameTree, QueryMiss) {
    NameTreeStore store;
    store.insertEvent(EventId{1}, parse(kEv1));
    store.insertEvent(EventId{2}, parse(kEv2));

    const char* q = R"({
      "names": {"name": {"name": "<qualified>", "cvr": []}, "params": [
        {"name": {"name": "std", "cvr": []}, "params": []},
        {"name": {"name": "vector", "cvr": []}, "params": [
          {"name": {"name": "int", "cvr": []}, "params": []}
        ]}
      ]}
    })";

    EXPECT_TRUE(toVec(store.query(parse(q))).empty());
}
