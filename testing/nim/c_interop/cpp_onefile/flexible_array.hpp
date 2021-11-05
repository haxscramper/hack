#include "nimbase.h"

struct ImGuiTable {
    int field = 0;
    ~ImGuiTable() {
    }
};


struct TGenericSeqQ {
    NI len;
    NI reserved;
};

struct ImSeq : TGenericSeqQ {
    ImGuiTable data[1];
};
