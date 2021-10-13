#pragma once
#define NIM_INTBITS 64
#include "nimbase.h"
#include <cstring>

using uint8  = unsigned short int;
using uint16 = unsigned int;
using uint32 = unsigned long int;
using uint64 = unsigned long long int;
using int8   = short int;
using int16  = int;
using int32  = long int;
using int64  = long long int;

struct NimClosureIter {
    void* impl;
    void* env;
};

struct NimClosureEnd {

};

N_INLINE(void, nimSetMem)(void* a, int v, NI size) {
    void* T1_;
    T1_ = (void*)0;
    T1_ = std::memset(a, v, ((size_t)(size)));
}

N_INLINE(void, nimZeroMem)(void* p, NI size) {
    nimSetMem(p, ((int)0), size);
}

struct TGenericSeq {
    NI len;
    NI reserved;
};

template <typename T>
struct NimSeq {
    TGenericSeq Sup;
    T           data[SEQ_DECL_SIZE];

    NimSeq() {
        nimZeroMem(this, sizeof(*this));
    }
    T& operator[](int idx) {
        return data[idx];
    }
};