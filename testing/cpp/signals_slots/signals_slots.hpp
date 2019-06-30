#include "../../common/cpp/bit_utils.hpp"
#include "../../common/cpp/log.hpp"
#include <cstring>
#include <functional>
#include <iostream>
#include <map>
#include <optional>
#include <typeinfo>
#include <utility>

class signal_base;

template <class T>
T& scastp(void* arg) {
    return *static_cast<T*>(arg);
}

namespace internal {
struct C {
    void member_method();
};


const size_t ptr_size = sizeof(&C::member_method);
using ptr_arr         = char[sizeof(&C::member_method)];

template <class T, typename... Args>
void memcpy(
    void (T::*method)(Args...),
    ptr_arr& pointer,
    bool     from_method) {
    if (from_method) {
        std::memcpy(&pointer, &method, ptr_size);
    } else {
        std::memcpy(&method, &pointer, ptr_size);
    }
}


} // namespace internal

struct slot_func {
    internal::ptr_arr pointer;

    template <class T, typename... Args>
    slot_func(void (T::*method)(Args...)) {
        internal::memcpy(method, pointer, true);
    }

    template <class T, typename... Args>
    void invoke(T* obj, Args... args) {
        METHOD_DBG

        void (T::*method)(Args...);

        internal::memcpy(method, pointer, false);

        LINE_DBG;

        bit_print(method);

        (obj->*method)(args...);
    }
};

/// Wrapper on class methods.
struct signal_func {
    internal::ptr_arr pointer;

    template <class T, typename... Args>
    signal_func(void (T::*method)(Args...)) {
        internal::memcpy(method, pointer, true);
    }

    bool operator<(const signal_func& rhs) const {
        return std::memcmp(
            &pointer, &rhs.pointer, sizeof(&internal::C::member_method));
    }
};

class signal_base
{
  private:
    struct signal_compare {
        bool operator()(const signal_func& lhs, const signal_func& rhs)
            const {
            return lhs < rhs;
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
        LOG << "Printing connections" << connects.size();
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

    template <typename... Args>
    void emit_signal(signal_func signal, Args... args) {
        METHOD_DBG

        LOG << "Emitting signal";

        // Find range of all handler-slot pairs that correspond to given
        // signal
        std::pair<signal_iter, signal_iter>
            equal_range = connects.equal_range(signal);

        LOG << "Number of connections"
            << std::distance(equal_range.first, equal_range.second);

        LOG << "Number of elements in map" << connects.size();

        // Consecutively call each signal
        for (signal_iter slot = equal_range.first;
             slot != equal_range.second;
             ++slot) {

            signal_base* target      = slot->second.first;
            slot_func*   slot_method = &slot->second.second;

            slot_method->invoke(target, std::forward<Args>(args)...);
        }
    }


  private:
    signal_map connects;
};

// TODO debug signal-slot-connection argument types
template <typename... Args>
void __connect(
    signal_base* emitter,
    signal_func  signal,
    signal_base* handler,
    slot_func    slot
#ifdef DEBUG
    ,
    signal_base::signal_debug_name names
#endif
) {

    emitter->set_connection(signal, handler, slot);

    LOG << "Connecting" << names.signal_name << "to" << names.slot_name;

#ifdef DEBUG
    emitter->set_connection_name(signal, names);
#endif
}

#ifdef DEBUG

#    define connect(emitter, signal, target, slot)                        \
        __connect(                                                        \
            emitter,                                                      \
            signal,                                                       \
            target,                                                       \
            slot,                                                         \
            {#emitter, #signal, #target, #slot});

#else

#    define connect(emitter, signal, target, slot)                        \
        __connect(emitter, signal, target, slot);


#endif
