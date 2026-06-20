#include "set_collection.hpp"

#include <benchmark/benchmark.h>
#include <random>
#include <vector>

namespace {

constexpr uint32_t kUniverseMax = 20'000'000;

// Build a collection: `numSets` sets, each of `setSize` values, drawn from
// a shared pool of `poolSize` distinct integers to create overlap.
SubsetCollection buildCollection(
    size_t   numSets,
    size_t   setSize,
    size_t   poolSize,
    uint32_t seed) {
    std::mt19937                            rng(seed);
    std::uniform_int_distribution<uint32_t> poolDist(0, kUniverseMax - 1);

    // Shared pool of values for overlap.
    std::vector<uint32_t> pool(poolSize);
    for (uint32_t& v : pool) { v = poolDist(rng); }

    std::uniform_int_distribution<size_t> idxDist(0, poolSize - 1);

    SubsetCollection c;
    for (size_t s = 0; s < numSets; ++s) {
        auto id = c.createSet();
        for (size_t i = 0; i < setSize; ++i) {
            c.add(id, pool[idxDist(rng)]);
        }
    }
    return c;
}

} // namespace

// --- Case 1: small number of large sets with high overlap ---

static void BM_FewLargeSets_Build(benchmark::State& state) {
    const size_t numSets  = 10;
    const size_t setSize  = static_cast<size_t>(state.range(0));
    const size_t poolSize = setSize * 2; // high overlap
    for (auto _ : state) {
        SubsetCollection c = buildCollection(
            numSets, setSize, poolSize, 1234);
        benchmark::DoNotOptimize(&c);
    }
    state.SetItemsProcessed(state.iterations() * numSets * setSize);
}
BENCHMARK(BM_FewLargeSets_Build)
    ->Arg(100'000)
    ->Arg(500'000)
    ->Arg(1'000'000)
    ->Unit(benchmark::kMillisecond);

static void BM_FewLargeSets_Intersection(benchmark::State& state) {
    const size_t     numSets  = 10;
    const size_t     setSize  = static_cast<size_t>(state.range(0));
    const size_t     poolSize = setSize * 2;
    SubsetCollection c = buildCollection(numSets, setSize, poolSize, 1234);
    c.optimizeAll();
    for (auto _ : state) {
        uint64_t card = c.intersectionCardinality(0, 1);
        benchmark::DoNotOptimize(card);
    }
}
BENCHMARK(BM_FewLargeSets_Intersection)
    ->Arg(100'000)
    ->Arg(500'000)
    ->Arg(1'000'000)
    ->Unit(benchmark::kMicrosecond);

// --- Case 2: millions of distinct integers, proportional number of sets
// --- ~2M distinct values in the pool, ~2M small sets referencing them.

static void BM_ManySets_Build(benchmark::State& state) {
    const size_t numSets  = static_cast<size_t>(state.range(0));
    const size_t setSize  = 16;        // small sets
    const size_t poolSize = 2'000'000; // millions of distinct integers
    for (auto _ : state) {
        SubsetCollection c = buildCollection(
            numSets, setSize, poolSize, 5678);
        benchmark::DoNotOptimize(&c);
    }
    state.SetItemsProcessed(state.iterations() * numSets * setSize);
}
BENCHMARK(BM_ManySets_Build)
    ->Arg(500'000)
    ->Arg(1'000'000)
    ->Arg(2'000'000)
    ->Unit(benchmark::kMillisecond);

static void BM_ManySets_SetsContainingAll(benchmark::State& state) {
    const size_t     numSets  = static_cast<size_t>(state.range(0));
    const size_t     setSize  = 16;
    const size_t     poolSize = 2'000'000;
    SubsetCollection c = buildCollection(numSets, setSize, poolSize, 5678);
    c.optimizeAll();

    // Query with a couple of values likely present in the pool.
    std::mt19937                            rng(99);
    std::uniform_int_distribution<uint32_t> dist(0, kUniverseMax - 1);
    std::vector<uint32_t>                   query = {dist(rng), dist(rng)};

    for (auto _ : state) {
        auto view = c.setsContainingAll(query);
        benchmark::DoNotOptimize(view.cardinality());
    }
}
BENCHMARK(BM_ManySets_SetsContainingAll)
    ->Arg(500'000)
    ->Arg(1'000'000)
    ->Arg(2'000'000)
    ->Unit(benchmark::kMicrosecond);

static void BM_ManySets_InvertedQueryAny(benchmark::State& state) {
    const size_t     numSets  = static_cast<size_t>(state.range(0));
    const size_t     setSize  = 16;
    const size_t     poolSize = 2'000'000;
    SubsetCollection c = buildCollection(numSets, setSize, poolSize, 5678);
    c.optimizeAll();

    std::mt19937                            rng(99);
    std::uniform_int_distribution<uint32_t> dist(0, kUniverseMax - 1);
    std::vector<uint32_t>                   query;
    for (int i = 0; i < 8; ++i) { query.push_back(dist(rng)); }

    for (auto _ : state) {
        auto view = c.setsContainingAny(query);
        benchmark::DoNotOptimize(view.cardinality());
    }
}
BENCHMARK(BM_ManySets_InvertedQueryAny)
    ->Arg(500'000)
    ->Arg(1'000'000)
    ->Arg(2'000'000)
    ->Unit(benchmark::kMicrosecond);

BENCHMARK_MAIN();
