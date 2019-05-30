#include <cstring>
#include <iostream>
#include <map>
#include <utility>

/*
  - [ ] TODO write hash function for unordered_multimap and use ti
*/

class signal_base;

using signal_msg  = void*;
using slot_func   = void (signal_base::*)(signal_msg);
using signal_func = void (signal_base::*)(signal_msg);

template <class T>
T& scast(void* arg) {
    return *static_cast<T*>(arg);
}


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


class signal_base
{
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


  public:
    void signal_1(signal_msg arg) {
        std::cout << "Executed signal 1\n";
        emit_signal(&signal_base::signal_1, arg);
    }

    void signal_2(signal_msg arg) {
        std::cout << "Executed signal 2\n";
        emit_signal(&signal_base::signal_2, arg);
    }

  public:
    void slot_1(signal_msg arg) {
        std::cout << "signal_basealled slot 1: "
                  << scast<std::string>(arg);
    }

    void slot_2(signal_msg arg) {
        std::cout << "signal_basealled slot 2: "
                  << scast<std::string>(arg);
    }

  private:
    signal_map connects;
};

int main() {
    std::cout << "test 1\n\n\n";

    signal_base c;

    c.set_connection(&signal_base::signal_1, &c, &signal_base::slot_1);
    std::string arg = "hello\n";
    c.signal_1(&arg);
}
