#include <iostream>
#include <iomanip>

int indent = 0;

#define PUSH_FMT                                                          \
    std::ios init(NULL);                                                  \
    init.copyfmt(std::cout);

#define POP_FMT std::cout.copyfmt(init);

#define CALL(name)                                                        \
    {                                                                     \
        PUSH_FMT;                                                         \
        std::cout << std::setw(4) << __LINE__ << " " << std::setfill('>') \
                  << std::setw(indent * 2) << "";                         \
        std::cout << std::setfill(' ');                                   \
        std::cout << " " << __FUNCTION__ << " " << name << std::endl;     \
        POP_FMT                                                           \
    }

#define CALL_IN(name)                                                     \
    CALL(name);                                                           \
    ++indent;

#define CALL_OUT(name)                                                    \
    --indent;                                                             \
    CALL(name);

#define SEP(name)                                                         \
    {                                                                     \
        PUSH_FMT                                                          \
        indent = 0;                                                       \
        std::cout << std::setfill('-') << std::setw(60) << " " << name    \
                  << std::endl;                                           \
        POP_FMT                                                           \
    }

struct Ind {
    Ind(const char* text = "") { CALL_IN(text); }
};

struct Ded {
    Ded(const char* text = "") { CALL_OUT(text); }
};

struct Basic {
    Basic(const char* arg) { CALL(arg); }
};

struct Compound {
    Ind   it;
    Basic it1 = Basic("it1");
    Basic it2 = Basic("it2");
    Ded   out;
};


int main() {
    SEP("compound call1");
    Compound();
    SEP("destructor calls");
    {
        Ind("1");
        {
            Ind("1");
            { Ind("2"); }
        }
    }
}
