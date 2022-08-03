struct Top {
    const int a = 16;
};

struct Left : virtual public Top {
    const int l = 32;
};

struct Right : virtual public Top {
    const int r = 64;
};

struct Bottom
    : public Left
    , public Right {
    const int b = 128;
};
