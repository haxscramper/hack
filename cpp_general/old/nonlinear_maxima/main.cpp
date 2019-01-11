#include <gtest/gtest.h>
#include "maxima.hpp"

#define RES_SOLITION                                                      \
    Solution res;                                                         \
    res.x = x;                                                            \
    res.y = y;                                                            \
    res.z = z;

#define GET_MAXIMA(expression, type)                                      \
    Solution res = get_maxima(                                            \
        param,                                                            \
        [](double x, double y, double z) {                                \
            RES_SOLITION;                                                 \
            res.res = expression;                                         \
            return res;                                                   \
        },                                                                \
        type);

#define DEBUG_MAXIMA_2                                                    \
    LOG << "[          ] res.x  " << res.x;                               \
    LOG << "[          ] res.y  " << res.y;                               \
    LOG << "[          ] res.res" << res.res;

#define DEBUG_MAXIMA_3                                                    \
    LOG << "[          ] res.x  " << res.x;                               \
    LOG << "[          ] res.y  " << res.y;                               \
    LOG << "[          ] res.z  " << res.z;                               \
    LOG << "[          ] res.res" << res.res;


TEST(TwoVariable, Num_1) {
    SearchParams param;

    GET_MAXIMA(
        (3 * x * x + x * y + 2 * y * y - x - 4 * y), SolutionType::Min);
    ASSERT_NEAR(0, res.x, 0.01);
    ASSERT_NEAR(1, res.y, 0.01);
    ASSERT_NEAR(-2, res.res, 0.01);
    DEBUG_MAXIMA_2
}

TEST(TwoVariable, Num_29) {
    SearchParams param;
    param.x_start = 0.0;
    param.y_start = 0.0;
    GET_MAXIMA(
        (1 + x - y) / (std::sqrt(1 + x * x + y * y)), SolutionType::Max);
    ASSERT_NEAR(1, res.x, 0.01);
    ASSERT_NEAR(-1, res.y, 0.01);
    ASSERT_NEAR(std::sqrt(3), res.res, 0.01);
    DEBUG_MAXIMA_2
}


int main(int argc, char** argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
