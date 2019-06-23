#define DEBUG
#include "signals_slots.hpp"

class signal_test : public signal_base
{
  public:
    void slot(int a, std::string b) {
        LOG << a << b;
    }

    void signal(int a, std::string b) {
        emit_signal(signal_cast(&signal_test::signal), a, b);
    }

    void signal_wrong_type(int hellO) {
    }
};


int main() {
    LOG << "Wip";

    signal_test t;
    connect(&t, &signal_test::signal, &t, &signal_test::slot);

    // FIXME does not fail to compile
    connect(&t, &signal_test::signal_wrong_type, &t, &signal_test::slot);

    t.signal(1, "hello");

    LOG << "Done";
}
