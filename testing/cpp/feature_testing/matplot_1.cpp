#include <cmath>
#include <matplot/matplot.h>
#include <random>

int main() {
    using namespace matplot;

    std::vector<std::vector<double>> Y = {
        {10, 9, 8, 7}, //
        {0, 2, 2, 2},  //
        {0, 0, 3, 3},  //
        {0, 0, 0, 2},
    };
    barstacked(Y);
    matplot::legend({"2017", "2018", "2019", "2020"});

    save("/tmp/result.png");
    return 0;
}
