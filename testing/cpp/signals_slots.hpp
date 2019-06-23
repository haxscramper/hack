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

using signal_msg = void*;

template <class T, typename... Args>
using signal_method_ptr = void (T::*)(Args...);

template <class T, typename... Args>
using slot_method_ptr = void (T::*)(Args...);

struct slot_func {
    template <class T, typename... Args>
    static slot_func to_slot_func(slot_method_ptr<T, Args...> method) {
        slot_func res;
        res.slot_ptr = &method;
        return res;
    }

    // TODO Implement without std::invoke to use on C++11 compiler
    template <typename... Args>
    void invoke(signal_base* obj, Args&&... args) {
        using ptr_type = slot_method_ptr<signal_base, Args...>*;
        // Casting slot pointer to method with required signature
        ptr_type method = static_cast<ptr_type>(slot_ptr);
        // Invoking method with argments
        std::invoke(*method, obj, std::forward<Args>(args)...);
    }

  private:
    void* slot_ptr;
};


struct signal_func {
    template <class T, typename... Args>
    static signal_func to_signal_func(
        signal_method_ptr<T, Args...> method) {
        signal_func res;
        res.signal_ptr = &method;
        return res;
    }

  private:
    void* signal_ptr;
};

template <class T, typename... Args>
signal_func signal_cast(signal_method_ptr<T, Args...> method) {
    return signal_func::to_signal_func(method);
}

class signal_base
{
  private:
    struct signal_compare {
        bool operator()(const signal_func& lhs, const signal_func& rhs)
            const {
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
        LOG << "Inserting connection";
        connects.insert({signal, {target, slot}});
    }

    template <typename... Args>
    void emit_signal(signal_func signal, Args... args) {
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
            slot_func    slot_method = slot->second.second;

            slot_method.invoke(target, std::forward<Args>(args)...);
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
    std::tuple<      //
        std::string, //
        std::string, //
        std::string, //
        std::string> names
#endif
) {
#ifdef DEBUG
    INFO << "Connecting" << std::get<1>(names) << "to"
         << std::get<3>(names);
#endif

    emitter->set_connection(signal, handler, slot);

    LOG << "Done connecting";
}

#ifdef DEBUG

#    define connect(emitter, signal, target, slot)                        \
        __connect(                                                        \
            emitter,                                                      \
            signal_func::to_signal_func(signal),                          \
            target,                                                       \
            slot_func::to_slot_func(slot),                                \
            {#emitter, #signal, #target, #slot});

#else

#    define connect(emitter, signal, target, slot)                        \
        __connect(                                                        \
            emitter,                                                      \
            signal_func::to_signal_func(signal),                          \
            target,                                                       \
            slot_func::to_slot_func(slot));


#endif
