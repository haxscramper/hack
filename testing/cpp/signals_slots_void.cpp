#include <cstring>
#include <iostream>
#include <map>
#include <utility>

/*
 */

/*
  - [ ] TODO write hash function for unordered_multimap and use it
*/

class signal_base;

template <class T>
T& scastp(void* arg) {
    return *static_cast<T*>(arg);
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
        connects.insert({signal, {target, slot}});
    }

    void emit_signal(signal_func signal, signal_msg data) {
        std::pair<signal_iter, signal_iter>
            equal_range = connects.equal_range(signal);

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
        std::cout << "Executed signal 1\n";
        emit_signal(signal_cast(&signal_test::signal_1), arg);
    }

    void signal_2(signal_msg arg) {
        std::cout << "Executed signal 2\n";
        emit_signal(signal_cast(&signal_test::signal_2), arg);
    }

  public:
    void slot_1(signal_msg arg) {
        std::cout << "signal_test called slot 1: "
                  << scastp<std::string>(arg);
    }

    void slot_2(signal_msg arg) {
        std::cout << "signal_test called slot 2: "
                  << scastp<std::string>(arg);
    }
};


int main() {
    signal_test c;

    connect(&c, &signal_test::signal_1, &c, &signal_test::slot_1);

    std::string arg = "hello\n";
    c.signal_1(&arg);

    std::cout << "done main\n";
}
