struct Top {
    const int a = 16;
};

struct Left : virtual public Top {
    const int l = 32;
};

struct Right : virtual public Top {
    const int r = 64;
};

struct BottomVirtual
    : public Left
    , public Right {
    const int b = 128;
};


struct TopNonv {
    const int    a = 16;
    virtual void base() {}
};

struct LeftNonv : public TopNonv {
    const int l = 32;
};

struct RightNonv : public TopNonv {
    const int r = 64;
};

struct BottomNonvirtual
    : public LeftNonv
    , public RightNonv {
    const int b = 128;
};

struct Bitsized {
    char four : 4;
    char three : 3;
    char one : 1;
}

int main() {
    BottomVirtual();
    BottomNonvirtual();
    Bitsized();
}
