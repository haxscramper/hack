#include "common.hpp"
#include <cmath>

bool grayp(int N, CR<Str> code) {
    int T   = code.size();
    int idx = 0;
    while (0 < T) {
        if (N < std::pow(2, T) / 2) {
            if (code[idx] == '0') {
                --T;
            } else {
                return false;
            }
        } else {
            if (code[idx] == '1') {
                --T;
            } else {
                return false;
            }
        }
    }

    return true;
}

TEST(grayp_code, gray) {
    EXPECT_TRUE(grayp(0, "0"));
    EXPECT_TRUE(grayp(1, "1"));
    EXPECT_TRUE(grayp(0, "00"));
    EXPECT_TRUE(grayp(2, "11"));
}

int main(int argc, char* argv[]) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
