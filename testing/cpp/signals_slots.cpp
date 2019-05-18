#include "../../common/cpp/log.hpp"
#include <cstring>
#include <iostream>
#include <map>
#include <utility>

/*
  - [ ] TODO write hash function for unordered_multimap and use ti
*/

class C;

using slot_func   = void (C::*)(std::string&);
using signal_func = void (C::*)(std::string&);


struct signal_compare {
    bool operator()(const signal_func& lhs, const signal_func& rhs) {
        return std::memcmp(&lhs, &rhs, sizeof(lhs)) < 0;
    }
};

using signal_map = std::multimap<
    signal_func,              //
    std::pair<C*, slot_func>, //
    signal_compare>;

using signal_iter = signal_map::iterator;

class C
{
    // API
  public:
    void set_connection(signal_func signal, C* target, slot_func slot) {
        connects.insert({signal, {target, slot}});
    }

    void emit_signal(signal_func signal, std::string& data) {
        std::pair<signal_iter, signal_iter>
            equal_range = connects.equal_range(signal);

        for (signal_iter slot = equal_range.first;
             slot != equal_range.second;
             ++slot) {

            C*          target = slot->second.first;
            signal_func signal = slot->second.second;

            (target->*signal)(data);
        }
    }


    // Signals
  public:
    void signal_1(std::string&) {
        LOG << "Executed signal 1";
    }
    void signal_2(std::string&) {
        LOG << "Executed signal 2";
        INFO << "test:";
    }

    // Slots
  public:
    void slot_1(std::string& arg) {
        LOG << "Called slot 1" << arg;
    }

    void slot_2(std::string& arg) {
        LOG << "Called slot 2" << arg;
    }


    // Special member functions
  public:
    C() {
    }

  private:
    signal_map connects;
};


int main() {
    C c;

    c.set_connection(&C::signal_1, &c, &C::slot_1);

    LOG << "Hello";
}
