#include "common.hpp"

template <typename IterIn, typename IterOut>
void deduplicate(IterIn begin, IterIn end, IterOut out) {
    auto prev = end;
    while (begin != end) {
        if (prev == end || *begin != *prev) {
            out  = *begin;
            prev = begin;
        }
        ++begin;
    }
}

template <typename T>
Vec<T> deduplicate(CR<Vec<T>> in) {
    Vec<T> out;
    deduplicate(in.begin(), in.end(), std::back_inserter(out));
    return out;
}

TEST(dedup_empty, dedup) {
    ASSERT_EQ(deduplicate(Vec<int>{}), Vec<int>{})
        << "empty deduplication failed";
}

TEST(dedup_one, dedup) {
    ASSERT_EQ(deduplicate(Vec<int>{1}), Vec<int>{1})
        << "deduplication of one element failed";
}

int main(int argc, char* argv[]) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
