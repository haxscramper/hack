#include "../../common/cpp/log.hpp"
#include <cstring>
#include <iostream>
#include <map>
#include <typeinfo>
#include <utility>


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

#ifdef DEBUG


#    define connect(emitter, signal, target, slot)                        \
        (emitter)->set_connection(                                        \
            static_cast<signal_func>(signal),                             \
            target,                                                       \
            static_cast<slot_func>(slot));                                \
        (emitter)->set_connection_name(                                   \
            static_cast<signal_func>(signal),                             \
            {#emitter, #signal, #target, #slot});

#elif

#    define connect(emitter, signal, target, slot)                        \
        (emitter)->set_connection(                                        \
            static_cast<signal_func>(signal),                             \
            target,                                                       \
            static_cast<slot_func>(slot))

#endif


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


#ifdef DEBUG

  public:
    struct signal_debug_name {
        std::string emitter_name;
        std::string signal_name;
        std::string target_name;
        std::string slot_name;
    };

    std::multimap<signal_func, signal_debug_name, signal_compare>
        connects_debug;

    void print_connections() {
        auto              it = connects_debug.begin();
        signal_debug_name names;
        if (it != connects_debug.end()) {
            names = (*it).second;
            std::cout << "    " << names.emitter_name << "\n";
        } else {
            return;
        }

        std::string ctx_signal_name = "";
        for (auto& connection : connects_debug) {
            if (connection.second.signal_name != ctx_signal_name) {
                ctx_signal_name = connection.second.signal_name;
                std::cout << "    `-> " << ctx_signal_name << "\n";
            }

            std::cout << "       `-> " << connection.second.target_name
                      << "\n"
                      << "           " << connection.second.slot_name
                      << "\n";
        }
    };

    void set_connection_name(
        signal_func       signal,
        signal_debug_name debug_name) {
        connects_debug.insert({signal, debug_name});
    }

#endif


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
