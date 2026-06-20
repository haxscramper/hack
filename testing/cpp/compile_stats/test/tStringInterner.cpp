#include "string_interner.hpp"

#include <gtest/gtest.h>
#include <string>

TEST(StringInterner, InternsUniqueStringsAndDeduplicates) {
    StringInterner interner;

    const auto id_hello_1    = interner.intern("hello");
    const auto id_world      = interner.intern("world");
    const auto id_hello_2    = interner.intern("hello");
    const auto id_world_copy = interner.intern(std::string("world"));

    EXPECT_EQ(id_hello_1, id_hello_2);
    EXPECT_EQ(id_world, id_world_copy);
    EXPECT_NE(id_hello_1, id_world);

    EXPECT_EQ(interner.size(), 2u);
    EXPECT_EQ(interner.get(id_hello_1), "hello");
    EXPECT_EQ(interner.get(id_world), "world");
}

TEST(StringInterner, GetThrowsForInvalidId) {
    StringInterner interner;
    interner.intern("only");

    EXPECT_THROW(
        static_cast<void>(interner.get(StrId{9999})), std::out_of_range);
}
