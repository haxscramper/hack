#include <bpf/libbpf.h>
#include <csignal>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <functional>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <nlohmann/json.hpp>
#include <fstream>
#include <atomic>
#include <iostream>

#include <google/protobuf/util/delimited_message_util.h>

#include <spdlog/spdlog.h>

#include <libproc2/pids.h>

#include "trace.skel.h"
#include "trace_event.pb.h"

struct Event {
    std::uint32_t type;
    std::uint32_t pid;
    std::uint32_t ppid;
    std::uint32_t uid;
    std::uint64_t ts;
    std::int32_t  exit_code;
    char          comm[16];
};

struct TrackedProcess {
    std::uint32_t            pid      = 0;
    std::uint32_t            ppid     = 0;
    std::uint32_t            uid      = 0;
    std::uint64_t            start_ts = 0;
    std::string              comm;
    std::string              cwd;
    std::vector<std::string> args;
    bool                     has_stop  = false;
    std::uint64_t            stop_ts   = 0;
    std::int32_t             exit_code = 0;
};

struct State {
    int                                     userUid;
    int                                     pageSize;
    int                                     hz;
    std::unordered_set<int>                 shellPids;
    std::unordered_map<int, TrackedProcess> processes;
    std::atomic_bool                        running;
    std::unique_ptr<std::ofstream>          outputStream;
};

static std::atomic_bool* runningRef = nullptr;

static void onSignal(int sig) {
    if (sig == SIGINT || sig == SIGTERM) {
        if (runningRef != nullptr) { runningRef->store(false); }
    }
}

static int getRealUserUid() {
    const char* sudoUid = std::getenv("SUDO_UID");
    if (sudoUid != nullptr) {
        try {
            return std::stoi(sudoUid);
        } catch (...) {
            throw std::runtime_error(
                std::format(
                    "Could not parse the UID from the SUDO_UID, not a "
                    "valid number '{}'",
                    sudoUid));
        }
    }
    return static_cast<int>(::getuid());
}

// -------------------------- procps utility --------------------------

struct ProcSnapshot {
    int                      pid     = 0;
    int                      ppid    = 0;
    int                      uid     = 0;
    std::uint64_t            startNs = 0;
    std::string              comm;
    std::string              cwd;
    std::vector<std::string> args;
};

static std::uint64_t ticksToNs(std::uint64_t ticks, int hz) {
    if (hz <= 0) { return 0; }
    return (ticks * 1000000000ULL) / static_cast<std::uint64_t>(hz);
}

enum class ProcQueryKind
{
    Basic,
    Full,
};

static enum pids_item basicItems[] = {
    PIDS_ID_PID,
    PIDS_ID_PPID,
    PIDS_ID_RUID,
    PIDS_CMD,
    PIDS_TIME_START,
};

static enum pids_item fullItems[] = {
    PIDS_ID_PID,
    PIDS_ID_PPID,
    PIDS_ID_RUID,
    PIDS_CMD,
    PIDS_TIME_START,
    PIDS_CMDLINE_V,
    PIDS_CGROUP_V,
};

static std::string readCwd(int pid) {
    std::error_code ec;
    auto            path = std::filesystem::read_symlink(
        std::format("/proc/{}/cwd", pid), ec);
    if (ec) { return {}; }
    return path.string();
}

static std::optional<ProcSnapshot> readProcSnapshotForPid(
    int           pid,
    State const&  state,
    ProcQueryKind kind) {
    enum pids_item* items;
    int             itemCount;

    switch (kind) {
        case ProcQueryKind::Basic:
            items     = basicItems;
            itemCount = sizeof(basicItems) / sizeof(basicItems[0]);
            break;
        case ProcQueryKind::Full:
            items     = fullItems;
            itemCount = sizeof(fullItems) / sizeof(fullItems[0]);
            break;
    }

    struct pids_info* info = nullptr;
    if (procps_pids_new(&info, items, itemCount) < 0) {
        return std::nullopt;
    }

    unsigned           pidArr[2] = {static_cast<unsigned>(pid), 0};
    struct pids_fetch* fetch     = procps_pids_select(
        info, pidArr, 1, PIDS_SELECT_PID);
    if (!fetch || fetch->counts->total == 0) {
        procps_pids_unref(&info);
        return std::nullopt;
    }

    struct pids_stack* stack = fetch->stacks[0];

    ProcSnapshot s;
    for (int i = 0; i < itemCount; ++i) {
        struct pids_result* res = &stack->head[i];
        switch (items[i]) {
            case PIDS_ID_PID: s.pid = res->result.s_int; break;
            case PIDS_ID_PPID: s.ppid = res->result.s_int; break;
            case PIDS_ID_RUID: s.uid = res->result.s_int; break;
            case PIDS_CMD:
                if (res->result.str) { s.comm = res->result.str; }
                break;
            case PIDS_TIME_START:
                s.startNs = ticksToNs(
                    static_cast<std::uint64_t>(res->result.ull_int),
                    state.hz);
                break;
            case PIDS_CMDLINE_V:
                if (res->result.strv) {
                    for (char** p = res->result.strv; *p; ++p) {
                        s.args.emplace_back(*p);
                    }
                }
                break;
            default: break;
        }
    }

    if (kind == ProcQueryKind::Full) { s.cwd = readCwd(pid); }

    procps_pids_unref(&info);
    return s;
}

template <typename Fn>
static void forEachProcSnapshot(
    State const&  state,
    ProcQueryKind kind,
    Fn&&          fn) {
    enum pids_item* items;
    int             itemCount;

    switch (kind) {
        case ProcQueryKind::Basic:
            items     = basicItems;
            itemCount = sizeof(basicItems) / sizeof(basicItems[0]);
            break;
        case ProcQueryKind::Full:
            items     = fullItems;
            itemCount = sizeof(fullItems) / sizeof(fullItems[0]);
            break;
    }

    struct pids_info* info = nullptr;
    if (procps_pids_new(&info, items, itemCount) < 0) {
        throw std::runtime_error("procps_pids_new() failed");
    }

    struct pids_fetch* fetch = procps_pids_reap(
        info, PIDS_FETCH_TASKS_ONLY);
    if (!fetch) {
        procps_pids_unref(&info);
        throw std::runtime_error("procps_pids_reap() failed");
    }

    for (int idx = 0; idx < fetch->counts->total; ++idx) {
        struct pids_stack* stack = fetch->stacks[idx];

        ProcSnapshot s;
        for (int i = 0; i < itemCount; ++i) {
            struct pids_result* res = &stack->head[i];
            switch (items[i]) {
                case PIDS_ID_PID: s.pid = res->result.s_int; break;
                case PIDS_ID_PPID: s.ppid = res->result.s_int; break;
                case PIDS_ID_RUID: s.uid = res->result.s_int; break;
                case PIDS_CMD:
                    if (res->result.str) { s.comm = res->result.str; }
                    break;
                case PIDS_TIME_START:
                    s.startNs = ticksToNs(
                        static_cast<std::uint64_t>(res->result.ull_int),
                        state.hz);
                    break;
                case PIDS_CMDLINE_V:
                    if (res->result.strv) {
                        for (char** p = res->result.strv; *p; ++p) {
                            s.args.emplace_back(*p);
                        }
                    }
                    break;
                default: break;
            }
        }

        if (kind == ProcQueryKind::Full) { s.cwd = readCwd(s.pid); }

        fn(s);
    }

    procps_pids_unref(&info);
}

// -------------------------- output --------------------------

static void writeFinalOutput(State& state) {
    // Build parent -> children tree
    std::unordered_map<int, std::vector<int>> children;
    for (const auto& [pid, proc] : state.processes) {
        children[proc.ppid].push_back(pid);
    }

    // Collect all descendants of target shells
    std::unordered_set<int>      selected;
    std::unordered_map<int, int> root_shell_map; // pid -> root shell pid

    std::function<void(int, int)> collect = [&](int pid, int root_shell) {
        auto proc_it = state.processes.find(pid);
        if (proc_it == state.processes.end()) { return; }
        if (selected.contains(pid)) { return; }

        selected.insert(pid);
        root_shell_map[pid] = root_shell;

        auto child_it = children.find(pid);
        if (child_it != children.end()) {
            for (int child : child_it->second) {
                collect(child, root_shell);
            }
        }
    };

    for (int shell_pid : state.shellPids) {
        collect(shell_pid, shell_pid);
    }

    SPDLOG_INFO("Writing {} processes to output", selected.size());

    // Sort by start timestamp for consistent ordering
    std::vector<int> output_pids(selected.begin(), selected.end());
    std::sort(output_pids.begin(), output_pids.end(), [&](int a, int b) {
        return state.processes.at(a).start_ts
             < state.processes.at(b).start_ts;
    });

    // Write all events
    for (int pid : output_pids) {
        const auto& proc = state.processes.at(pid);

        // Write start event
        procmon::Event start_evt;
        auto*          start = start_evt.mutable_start();
        start->set_timestamp_ns(proc.start_ts);
        start->set_pid(proc.pid);
        start->set_ppid(proc.ppid);
        start->set_uid(proc.uid);
        start->set_root_shell_pid(root_shell_map.at(pid));
        start->set_comm(proc.comm);
        start->set_cwd(proc.cwd);
        for (const auto& arg : proc.args) { start->add_args(arg); }

        // Add parent info if parent exists in our records
        auto parent_it = state.processes.find(proc.ppid);
        if (parent_it != state.processes.end()) {
            start->set_parent_cwd(parent_it->second.cwd);
            for (const auto& arg : parent_it->second.args) {
                start->add_parent_args(arg);
            }
        }

        if (!google::protobuf::util::SerializeDelimitedToOstream(
                start_evt, state.outputStream.get())) {
            throw std::runtime_error("Failed to serialize start event");
        }

        // Write stop event if process has exited
        if (proc.has_stop) {
            procmon::Event stop_evt;
            auto*          stop = stop_evt.mutable_stop();
            stop->set_timestamp_ns(proc.stop_ts);
            stop->set_pid(proc.pid);
            stop->set_comm(proc.comm);
            stop->set_exit_code(proc.exit_code);
            stop->set_duration_ns(proc.stop_ts - proc.start_ts);

            if (!google::protobuf::util::SerializeDelimitedToOstream(
                    stop_evt, state.outputStream.get())) {
                throw std::runtime_error("Failed to serialize stop event");
            }
        }
    }

    state.outputStream->flush();
}

// -------------------------- bootstrap --------------------------

static void bootstrap(
    State&                                 state,
    const std::optional<std::vector<int>>& target_pids_opt = std::
        nullopt) {

    // Load all existing processes into tracking
    forEachProcSnapshot(
        state, ProcQueryKind::Full, [&](ProcSnapshot const& s) {
            if (s.pid <= 0) { return; }

            TrackedProcess tp;
            tp.pid      = s.pid;
            tp.ppid     = s.ppid;
            tp.uid      = s.uid;
            tp.start_ts = s.startNs;
            tp.comm     = s.comm;
            tp.cwd      = s.cwd;
            tp.args     = s.args;

            state.processes.insert_or_assign(s.pid, std::move(tp));
        });

    // Determine target shell PIDs
    if (target_pids_opt.has_value()) {
        for (int pid : target_pids_opt.value()) {
            state.shellPids.insert(pid);
        }
    } else {
        for (const auto& [pid, proc] : state.processes) {
            if (proc.comm == "elvish") {
                state.shellPids.insert(pid);
                SPDLOG_INFO(
                    "Found target shell PID {} ({})", pid, proc.comm);
            }
        }
    }

    if (state.shellPids.empty()) {
        throw std::runtime_error(
            "Could not find any target processes (shells or specified "
            "PIDs)");
    }

    SPDLOG_INFO("Tracking {} target shell(s)", state.shellPids.size());
}

// -------------------------- event handling --------------------------

static int handleEvent(void* ctx, void* data, size_t dataSize) {
    if (dataSize < sizeof(Event)) {
        throw std::runtime_error{"event size mismatch"};
    }
    State&       state = *static_cast<State*>(ctx);
    Event const& e     = *static_cast<Event const*>(data);

    int pid = static_cast<int>(e.pid);

    if (e.type == 0) {
        // Process fork event
        TrackedProcess tp;
        tp.pid      = e.pid;
        tp.ppid     = e.ppid;
        tp.uid      = e.uid;
        tp.start_ts = e.ts;
        tp.comm     = std::string{e.comm};

        // Get additional details from /proc
        auto snap = readProcSnapshotForPid(
            pid, state, ProcQueryKind::Full);
        if (snap) {
            tp.cwd  = snap->cwd;
            tp.args = snap->args;
        }

        state.processes.insert_or_assign(pid, std::move(tp));
        SPDLOG_TRACE("Tracked start of PID {} ({})", pid, tp.comm);

    } else if (e.type == 1) {
        // Process exit event
        auto it = state.processes.find(pid);
        if (it != state.processes.end()) {
            it->second.has_stop  = true;
            it->second.stop_ts   = e.ts;
            it->second.exit_code = e.exit_code;
            SPDLOG_TRACE(
                "Tracked stop of PID {} (exit_code: {})",
                pid,
                e.exit_code);
        } else {
            // Missed the fork, but record the exit with available info
            TrackedProcess tp;
            tp.pid      = e.pid;
            tp.ppid     = e.ppid;
            tp.uid      = e.uid;
            tp.start_ts = e.ts; // Use exit time as approximate start if
                                // unknown
            tp.comm      = std::string{e.comm};
            tp.has_stop  = true;
            tp.stop_ts   = e.ts;
            tp.exit_code = e.exit_code;
            state.processes.insert_or_assign(pid, std::move(tp));
        }
    }

    return 0;
}

// -------------------------- main --------------------------

int main(int argc, char* argv[]) {
    libbpf_set_strict_mode(LIBBPF_STRICT_ALL);

    State state{
        .userUid      = getRealUserUid(),
        .pageSize     = static_cast<int>(::getpagesize()),
        .hz           = static_cast<int>(::sysconf(_SC_CLK_TCK)),
        .shellPids    = {},
        .processes    = {},
        .running      = true,
        .outputStream = nullptr,
    };

    runningRef = &state.running;
    std::signal(SIGINT, onSignal);
    std::signal(SIGTERM, onSignal);

    spdlog::set_level(spdlog::level::info);
    spdlog::set_pattern("[%l] %v");

    std::cout << "Process tracing started. Press Ctrl-C to stop and write "
                 "output...\n";

    if (argc < 2) {
        throw std::runtime_error(
            "Configuration required as first argument (JSON file or "
            "literal)");
    }

    std::string    arg = argv[1];
    nlohmann::json j;

    if (std::filesystem::exists(arg)) {
        std::ifstream file(arg);
        if (!file) {
            throw std::runtime_error(
                std::format("Failed to open file: {}", arg));
        }
        file >> j;
    } else {
        j = nlohmann::json::parse(arg);
    }

    if (!j.contains("output_file")) {
        throw std::runtime_error(
            "Configuration must contain 'output_file' field");
    }

    std::string outputPath = j["output_file"];
    state.outputStream     = std::make_unique<std::ofstream>(
        outputPath, std::ios::binary);
    if (!state.outputStream->is_open()) {
        throw std::runtime_error(
            std::format("Failed to open output file: {}", outputPath));
    }

    std::optional<std::vector<int>> target_pids;
    if (j.contains("target_pids") && j["target_pids"].is_array()) {
        target_pids = j["target_pids"].get<std::vector<int>>();
    }

    trace_bpf* skel = trace_bpf__open_and_load();
    if (skel == nullptr) {
        throw std::runtime_error{"failed to open and load bpf skeleton"};
    }
    int attachErr = trace_bpf__attach(skel);
    if (attachErr < 0) {
        throw std::runtime_error{
            std::format("failed to attach bpf programs: {}", attachErr)};
    }

    bootstrap(state, target_pids);

    int          eventsFd = bpf_map__fd(skel->maps.events);
    ring_buffer* rb       = ring_buffer__new(
        eventsFd, handleEvent, &state, nullptr);
    if (rb == nullptr) {
        throw std::runtime_error{"failed to create ring buffer"};
    }

    while (state.running.load()) {
        int err = ring_buffer__poll(rb, 250);
        if (err < 0) {
            throw std::runtime_error{
                std::format("ring buffer poll failed: {}", err)};
        }
    }

    SPDLOG_INFO("Interrupted, writing final output...");

    ring_buffer__free(rb);
    trace_bpf__destroy(skel);

    writeFinalOutput(state);

    if (state.outputStream) { state.outputStream->close(); }
    SPDLOG_INFO("Closed output stream");

    return 0;
}
