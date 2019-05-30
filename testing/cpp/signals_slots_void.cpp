#include "../../common/cpp/log.hpp"
#include <cstring>
#include <iostream>
#include <map>
#include <typeinfo>
#include <utility>

/*
 */

/*
  - [ ] TODO write hash function for unordered_multimap and use it
*/

class signal_base;

template <class T>
T& scastp(void* arg) {
    // try {
    return *static_cast<T*>(arg);
    // } catch (std::bad_alloc& e) {
    //     throw std::invalid_argument(
    //         std::string("Cannot convert from void* to ")
    //         + typeid(T).name());
    // }
}

using signal_msg  = void*;
using slot_func   = void (signal_base::*)(signal_msg);
using signal_func = void (signal_base::*)(signal_msg);


template <class C>
slot_func slot_cast(void (C::*func)(signal_msg)) {
    return static_cast<slot_func>(func);
}

template <class C>
signal_func signal_cast(void (C::*func)(signal_msg)) {
    return static_cast<signal_func>(func);
}

#define connect(emitter, signal, target, slot)                            \
    (emitter)->set_connection(                                            \
        static_cast<signal_func>(signal),                                 \
        target,                                                           \
        static_cast<slot_func>(slot))


class signal_base
{
  private:
    struct signal_compare {
        bool operator()(const signal_func& lhs, const signal_func& rhs) {
            return std::memcmp(&lhs, &rhs, sizeof(lhs)) < 0;
        }
    };

    using signal_map = std::multimap<
        signal_func,
        std::pair<signal_base*, slot_func>,
        signal_compare>;

    using signal_iter = signal_map::iterator;


  public:
    void set_connection(
        signal_func  signal,
        signal_base* target,
        slot_func    slot) {
        LOG << "Adding connection";
        connects.insert({signal, {target, slot}});
    }

    void emit_signal(signal_func signal, signal_msg data) {
        std::pair<signal_iter, signal_iter>
            equal_range = connects.equal_range(signal);

        LOG << "Emitting signal";
        LOG << "Number of targets: "
            << std::distance(equal_range.first, equal_range.second);
        for (signal_iter slot = equal_range.first;
             slot != equal_range.second;
             ++slot) {

            signal_base* target = slot->second.first;
            signal_func  signal = slot->second.second;

            (target->*signal)(data);
        }
    }


  private:
    signal_map connects;
};

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
