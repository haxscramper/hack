#include "cxx_opcodes.hpp"
#include "timeit.hpp"
#include <algorithm>
#include <random>
#include <string>
#include <vector>

const int Nb_Instructions = 1000000;
enum
{
    OPINC,
    OPDEC,
    OPMUL2,
    OPDIV2,
    OPADD7,
    OPNEG
};
std::random_device
    rd; //Will be used to obtain a seed for the random number engine
std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
std::uniform_int_distribution<> distrib(OPINC, OPNEG);

Opc* genOp() {
    switch (distrib(gen)) {
        case OPINC: return new OpcInc();
        case OPDEC: return new OpcDec();
        case OPMUL2: return new OpcMul2();
        case OPDIV2: return new OpcDiv2();
        case OPADD7: return new OpcAdd7();
        case OPNEG: return new OpcNeg();
    };
}

int main(void) {
    std::vector<Opc*> vecOps;
    //vecOps.reserve(Nb_Instructions);
    for (int x = 0; x < Nb_Instructions; ++x) {
        vecOps.push_back(genOp());
    }

    timeit(
        [&] {
            int result = 100;
            for (auto op : vecOps) {
                op->eval(&result);
            }
        },
        607);
}
