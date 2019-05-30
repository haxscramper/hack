#include "signals_slots.hpp"


struct custom_data {
    int         number;
    std::string string = "hello world";
};

class signal_test : public signal_base
{
  public:
    void signal_1(signal_msg arg) {
        LOG << "Executed signal 1";
        emit_signal(signal_cast(&signal_test::signal_1), arg);
    }

    void signal_2(signal_msg arg) {
        LOG << "Executed signal 2";
        emit_signal(signal_cast(&signal_test::signal_2), arg);
    }

    void signal_custom_data(signal_msg data) {
        LOG << "Executed custom data signal";
        LOG << "data.string: " << scastp<custom_data>(data).string;
        emit_signal(signal_cast(&signal_test::signal_custom_data), data);
    }

  public:
    void slot_1(signal_msg arg) {
        LOG << "signal_test called slot 1: " << scastp<std::string>(arg);
    }

    void slot_2(signal_msg arg) {
        LOG << "signal_test called slot 2: " << scastp<std::string>(arg);
    }

    void slot_custom_data(signal_msg arg) {
        LOG << "Custom data slot called";
        LOG << "arg.string" << scastp<custom_data>(arg).string;
    }
};


int main() {
    signal_test c;

    connect(
        &c,
        &signal_test::signal_1, //
        &c,
        &signal_test::slot_1);


    connect(
        &c,
        &signal_test::signal_1, //
        &c,
        &signal_test::slot_2);


    std::string arg = "hello";
    c.signal_1(&arg);

    LOG << "----";

    connect(
        &c,
        &signal_test::signal_custom_data,
        &c,
        &signal_test::slot_custom_data);

    custom_data test;

    c.signal_custom_data(&test);

    LOG << "done main";
}
