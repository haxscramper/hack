#pragma once

#include <assert.h>
#include <chrono>
#include <iomanip>
#include <iostream>
#include <math.h>
#include <sstream>
#include <thread> // we need it for this_thread::sleep_for to warm up the code
#include <vector>

#ifdef NDEBUG
#    define __timeit__noexcept noexcept
#else
#    define __timeit__noexcept
#endif

namespace _timeit {
bool autoprint = true;

using namespace std;

using namespace chrono;
using timer    = high_resolution_clock;
using IterType = long long;
using RepType  = long;

template <typename Collection>
string _join(
    const Collection& collection,
    const string&     delimiter = ", ") {
    ostringstream out;
    out << collection[0];
    for (const auto& data : collection) {
        out << delimiter;
        out << data;
    }
    return out.str();
}

time_t _ns(const timer::duration& duration) {
    return duration_cast<nanoseconds>(duration).count();
};

namespace _granularity {
    /** A minimum measurable time */
    time_t _granularity() {
        auto start = timer::now();
        auto end   = timer::now();
        while (_ns(end - start) <= 0) {
            end = timer::now();
        };
        return _ns(end - start);
    }

    /** Get the minimal one between 50 calls. Just for reliablitity. */
    time_t _min_of_50_times() {
        time_t min_value = _granularity();
        for (int i = 0; i < 49; i++) {
            time_t value = _granularity();
            if (value < min_value) {
                min_value = value;
            }
        }
        return min_value;
    }

    /** "memoized"/cached lazy-initialized value */
    time_t get() {
        static time_t _cache = -1;

        if (_cache != -1) {
            return _cache;
        }

        // believe it or not, but we have to 'warm up' the program this way, even if we run
        // granularity code about 50 times!!!
        // It seems, that, initially, the app is not in CPU cache and it runs the
        // next code ~10-15 times slower, so I got the wrong granularity.
        // For some reasons, it's especially noticable in 'debug' configuration (in MSVS).
        // on my machine (Win 8 + MSVS) even 1 nanosecond looks ok, but I leave 1us just in case.
        this_thread::sleep_for(std::chrono::microseconds(1));

        _cache = _min_of_50_times();
        return _cache;
    }
} // namespace _granularity

template <typename T>
time_t time(T code, IterType n_iterations = 0) {

    auto start = timer::now();
    for (IterType i = 0; i < n_iterations; i++) {
        code();
    }
    auto end = timer::now();
    return _ns(end - start);
}

template <typename T>
IterType num_best_iters(T code, IterType iterations) {
    if (iterations <= 0) {
        for (iterations = 1; iterations < 10000000000; iterations *= 10) {
            time_t time_to_perform = time(code, iterations);

            if (time_to_perform >= _granularity::get() * 1000) {
                break;
            }
        }
    }
    return iterations;
}

struct Stats {
    struct DecomposedTime {
        const int ps, ns, us,
            ms, // ps=picoseconds, us - microseconds, ms - milliseconds
            _s, mm, hh, dd;

        /** I use long double because I don't really know how people will use this class.
            Probably, some people will run it during several days. Probably, someone will run it during a year.
            We can't count picoseconds during one year in long-long: we need 64.77 bits for that.
            long-double just looses precision, but long-long just overflows and looses the data completely.
            For sure, I don't believe, someone will run this program for a year, but... */
        DecomposedTime(long double nanoseconds) __timeit__noexcept
            : ps{static_cast<int>(fmodl(nanoseconds * 1000.L, 1000.L))}
            , ns{static_cast<int>(fmodl(nanoseconds, 1000.L))}
            , us{static_cast<int>(fmodl(nanoseconds / 1000.L, 1000.L))}
            , ms{static_cast<int>(fmodl(nanoseconds / 1000000.L, 1000.L))}
            , _s{static_cast<int>(fmodl(nanoseconds / 1000000000.L, 60.L))}
            , mm{static_cast<int>(
                  fmodl(nanoseconds / (1000000000.L * 60.L), 60.L))}
            , hh{static_cast<int>(
                  fmodl(nanoseconds / (1000000000.L * 60.L * 60.L), 24.L))}
            , dd{static_cast<int>(
                  nanoseconds / (1000000000.L * 60.L * 60.L * 24.L))} {
            assert(nanoseconds > 0);
        }

        operator string() const {
            // != 0 is used instead of > 0, to ensure that I don't have negatives numbers.
            // That's also the reason why I don't use unsigned types here
            ostringstream out;
            out << fixed;
            if (dd) {
                out << dd << " days, "; // << hh << 'h';
                                        //return out.str();
            }

            if (hh || mm) {
                if (hh) {
                    // 2:15:12
                    out << hh << ':' << setfill('0') << setw(2) << mm
                        << ':' << setfill('0') << setw(2) << _s;
                    return out.str();
                }

                // 1:00.025
                out << mm << ':' << setfill('0') << setw(2) << _s << '.'
                    << setfill('0') << setw(3) << ms;
                return out.str();
            }

            if (_s)
                return _fmt(out, _s, ms, "s"); // 1.025s
            if (ms)
                return _fmt(out, ms, us, "ms"); // 12.026ms
            if (us)
                return _fmt(out, us, ns, "us"); // 139.051us
            if (ns)
                return _fmt(out, ns, ps, "ns"); // 1.050ns
            out << ps << "ps";                  // 570ps
            return out.str();
        }

        friend ostream& operator<<(
            ostream&              out,
            const DecomposedTime& time) {
            out << static_cast<string>(time);
            return out;
        }

      private:
        /** _fmt(out, 1, 23, "ms") -> "1.023ms" */
        static string _fmt(
            ostringstream& out,
            time_t         integral,
            time_t         fractional,
            const string&  unit) {
            out << integral << '.' << setfill('0') << setw(3) << fractional
                << unit;
            return out.str();
        }
    };


    Stats(IterType iterations, RepType repetitions)
        : _n_iterations(iterations)
        , _n_repetitions(repetitions)
        , _repetitions_ns(_n_repetitions){};
    IterType            _n_iterations;
    RepType             _n_repetitions;
    vector<long double> _repetitions_ns;

    // output stats
    long double mean = 0;
    long double fast = -1;

    Stats& operator<<(time_t nanos) {
        long double _ns_per_repetition = static_cast<long double>(nanos)
                                         / static_cast<long double>(
                                             _n_iterations);
        _repetitions_ns.push_back(_ns_per_repetition);

        mean += _ns_per_repetition;

        if (_ns_per_repetition < fast || fast < 0) {
            fast = _ns_per_repetition;
        }

        return *this;
    }

    operator string() const {
        stringstream s;
        s << fixed << setprecision(2);
        s << "min: " << DecomposedTime(fast)
          << ", mean: " << DecomposedTime(mean / _n_repetitions) << " ("
          << _n_repetitions << " runs, " << _n_iterations
          << " loops each)";
        return s.str();
    }

    friend ostream& operator<<(ostream& out, const Stats& stats) {
        out << static_cast<string>(stats);
        return out;
    }
};

template <class T>
Stats timeit(T code, int repetitions = 3, IterType iterations = 0) {

    iterations = num_best_iters(code, iterations);

    Stats stats(iterations, repetitions);

    for (int i = 0; i < repetitions; i++) {
        stats << time(code, iterations);
    }

    if (autoprint) {
        cout << (string)stats << endl;
    }
    return stats;
}
} // namespace _timeit

using _timeit::timeit;
