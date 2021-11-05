#pragma once
#include "nimbase.h"

typedef struct {
    int field;
} ImGuiTable;

typedef struct {
    NI len;
    NI capacity;
} SeqData;

typedef struct {
    SeqData     Sup;
    ImGuiTable* data;
} ImSeq;
