#include <bpf/libbpf.h>
#include <chrono>
#include <csignal>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <nlohmann/json.hpp>
#include <fstream>
#include <filesystem>
#include <optional>
#include <vector>
#include <atomic>
#include <iostream>
#include <fmt/ranges.h>

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

struct ProcInfo {
    int                      pid;
    int                      ppid;
    int                      uid;
    int                      rootShellPid;
    std::uint64_t            startNs;
    std::string              comm;
    std::string              cwd;
    std::vector<std::string> args;
    std::string              parentCwd;
    std::vector<std::string> parentArgs;
};

struct State {
    int                               userUid;
    int                               pageSize;
    int                               hz;
    int                               sampleMs;
    std::unordered_set<int>           shellPids;
    std::unordered_map<int, ProcInfo> processes;
    std::mutex                        mutex;
    std::atomic_bool                  running;
    std::unique_ptr<std::ofstream>    outputStream;
    std::mutex                        outputMutex;
    bool                              trackMemory;
};

static std::atomic_bool* runningRef = nullptr;

static void onSignal(int sig) {
    if (sig == SIGINT || sig == SIGTERM) {
        SPDLOG_INFO("Stopping process tracking");
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
    std::uint64_t            rssKb   = 0;
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
    Mem,
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

static enum pids_item memItems[] = {
    PIDS_ID_PID,
    PIDS_ID_PPID,
    PIDS_ID_RUID,
    PIDS_CMD,
    PIDS_TIME_START,
    PIDS_MEM_RES,
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
        case ProcQueryKind::Mem:
            items     = memItems;
            itemCount = sizeof(memItems) / sizeof(memItems[0]);
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
            case PIDS_MEM_RES:
                s.rssKb = static_cast<std::uint64_t>(res->result.ul_int);
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
        case ProcQueryKind::Mem:
            items     = memItems;
            itemCount = sizeof(memItems) / sizeof(memItems[0]);
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
                case PIDS_MEM_RES:
                    s.rssKb = static_cast<std::uint64_t>(
                        res->result.ul_int);
                    break;
                default: break;
            }
        }

        if (kind == ProcQueryKind::Full) { s.cwd = readCwd(s.pid); }

        fn(s);
    }

    procps_pids_unref(&info);
}

// -------------------------- protobuf output --------------------------

static void writeEvent(State& state, const procmon::Event& event) {
    std::scoped_lock lock(state.outputMutex);
    if (!google::protobuf::util::SerializeDelimitedToOstream(
            event, state.outputStream.get())) {
        throw std::runtime_error(
            "Failed to serialize event to output stream");
    }
    state.outputStream->flush();
}

static void recordStart(State& state, ProcInfo const& info) {
    procmon::Event event;
    auto*          start = event.mutable_start();
    start->set_timestamp_ns(info.startNs);
    start->set_pid(info.pid);
    start->set_ppid(info.ppid);
    start->set_uid(info.uid);
    start->set_root_shell_pid(info.rootShellPid);
    start->set_comm(info.comm);
    start->set_cwd(info.cwd);
    for (const auto& arg : info.args) { start->add_args(arg); }
    start->set_parent_cwd(info.parentCwd);
    for (const auto& arg : info.parentArgs) {
        start->add_parent_args(arg);
    }
    writeEvent(state, event);
    SPDLOG_TRACE(
        "Started process {} {} {}", info.pid, info.comm, info.args);
}

static void recordStop(
    State&          state,
    ProcInfo const& info,
    std::uint64_t   stopNs,
    std::int32_t    exitCode) {
    procmon::Event event;
    auto*          stop = event.mutable_stop();
    stop->set_timestamp_ns(stopNs);
    stop->set_pid(info.pid);
    stop->set_comm(info.comm);
    stop->set_exit_code(exitCode);
    stop->set_duration_ns(stopNs - info.startNs);
    writeEvent(state, event);
}

// -------------------------- bootstrap --------------------------

struct BasicProc {
    int           ppid = 0;
    int           uid  = 0;
    std::string   comm;
    std::uint64_t startNs = 0;
};

static void bootstrap(
    State&                                 state,
    const std::optional<std::vector<int>>& target_pids_opt = std::
        nullopt) {
    std::unordered_map<int, BasicProc> basic;
    SPDLOG_INFO("State user ID:{}", state.userUid);

    forEachProcSnapshot(
        state, ProcQueryKind::Basic, [&](ProcSnapshot const& s) {
            if (s.comm.find("elvish") != std::string::npos) {
                SPDLOG_INFO(
                    "proc snapshot {} PID:{} UID:{}",
                    s.comm,
                    s.pid,
                    s.uid);
            }

            if (s.pid <= 0) { return; }
            if (s.uid != state.userUid) { return; }

            basic.insert_or_assign(
                s.pid,
                BasicProc{
                    .ppid    = s.ppid,
                    .uid     = s.uid,
                    .comm    = s.comm,
                    .startNs = s.startNs,
                });

            if (!target_pids_opt.has_value() && s.comm == "elvish") {
                state.shellPids.insert(s.pid);
            }
        });

    if (target_pids_opt.has_value()) {
        for (int pid : target_pids_opt.value()) {
            state.shellPids.insert(pid);
        }
    }

    for (auto const& [pid, b] : basic) {
        if (state.shellPids.contains(pid)) { continue; }

        int ppid = b.ppid;

        int root = 0;
        int cur  = ppid;
        while (cur > 0) {
            if (state.shellPids.contains(cur)) {
                root = cur;
                break;
            }
            auto it = basic.find(cur);
            if (it == basic.end()) { break; }
            cur = it->second.ppid;
        }
        if (root <= 0) { continue; }

        auto childSnapOpt = readProcSnapshotForPid(
            pid, state, ProcQueryKind::Full);
        if (!childSnapOpt) { continue; }

        auto parentSnapOpt = readProcSnapshotForPid(
            ppid, state, ProcQueryKind::Full);

        ProcSnapshot const& child = *childSnapOpt;
        ProcSnapshot        parent{};
        if (parentSnapOpt) { parent = *parentSnapOpt; }

        ProcInfo info{
            .pid          = pid,
            .ppid         = ppid,
            .uid          = state.userUid,
            .rootShellPid = root,
            .startNs      = child.startNs,
            .comm         = basic.at(pid).comm.empty() ? child.comm
                                                       : basic.at(pid).comm,
            .cwd          = child.cwd,
            .args         = child.args,
            .parentCwd    = parent.cwd,
            .parentArgs   = parent.args,
        };

        state.processes.insert_or_assign(pid, std::move(info));
        recordStart(state, state.processes.at(pid));
    }
}

// -------------------------- event handling --------------------------

static int handleEvent(void* ctx, void* data, size_t dataSize) {
    if (dataSize < sizeof(Event)) {
        throw std::runtime_error{"event size mismatch"};
    }
    State&       state = *static_cast<State*>(ctx);
    Event const& e     = *static_cast<Event const*>(data);

    int pid  = static_cast<int>(e.pid);
    int ppid = static_cast<int>(e.ppid);
    int uid  = static_cast<int>(e.uid);

    if (uid != state.userUid) { return 0; }

    std::string comm = std::string{e.comm};

    if (e.type == 0) {
        std::scoped_lock<std::mutex> lock{state.mutex};


        int root = 0;
        if (state.shellPids.contains(ppid)) {
            root = ppid;
        } else if (state.processes.contains(ppid)) {
            root = state.processes.at(ppid).rootShellPid;
        }
        if (root <= 0) { return 0; }

        SPDLOG_TRACE("Launched process {} under {}", pid, ppid);

        auto childSnapOpt = readProcSnapshotForPid(
            pid, state, ProcQueryKind::Full);
        auto parentSnapOpt = readProcSnapshotForPid(
            ppid, state, ProcQueryKind::Full);

        ProcSnapshot child{};
        ProcSnapshot parent{};
        if (childSnapOpt) { child = *childSnapOpt; }
        if (parentSnapOpt) { parent = *parentSnapOpt; }

        ProcInfo info{
            .pid          = pid,
            .ppid         = ppid,
            .uid          = uid,
            .rootShellPid = root,
            .startNs      = static_cast<std::uint64_t>(e.ts),
            .comm         = comm,
            .cwd          = child.cwd,
            .args         = child.args,
            .parentCwd    = parent.cwd,
            .parentArgs   = parent.args,
        };

        SPDLOG_TRACE("Added process to tracking {}", pid);
        state.processes.insert_or_assign(pid, std::move(info));
        recordStart(state, state.processes.at(pid));
        return 0;
    }

    if (e.type == 1) {
        std::scoped_lock<std::mutex> lock{state.mutex};
        if (!state.processes.contains(pid)) { return 0; }
        ProcInfo info = state.processes.at(pid);
        recordStop(
            state, info, static_cast<std::uint64_t>(e.ts), e.exit_code);
        state.processes.erase(pid);
        return 0;
    }

    return 0;
}

// -------------------------- sampler --------------------------

static void samplerThread(State& state) {
    if (!state.trackMemory) { return; }

    while (state.running.load()) {
        std::this_thread::sleep_for(
            std::chrono::milliseconds{state.sampleMs});

        std::vector<int> pids;
        {
            std::scoped_lock<std::mutex> lock{state.mutex};
            pids.reserve(static_cast<int>(state.processes.size()));
            for (auto const& kv : state.processes) {
                pids.push_back(kv.first);
            }
        }

        auto now   = std::chrono::steady_clock::now();
        auto nowNs = std::chrono::duration_cast<std::chrono::nanoseconds>(
                         now.time_since_epoch())
                         .count();

        for (int pid : pids) {
            auto snapOpt = readProcSnapshotForPid(
                pid, state, ProcQueryKind::Mem);
            if (!snapOpt) { continue; }

            std::uint64_t rssKb = snapOpt->rssKb;
            std::string   comm;

            {
                std::scoped_lock<std::mutex> lock{state.mutex};
                if (state.processes.contains(pid)) {
                    comm = state.processes.at(pid).comm;
                } else {
                    continue;
                }
            }

            procmon::Event event;
            auto*          mem = event.mutable_memory();
            mem->set_timestamp_ns(static_cast<uint64_t>(nowNs));
            mem->set_pid(pid);
            mem->set_comm(comm);
            mem->set_rss_kb(rssKb);

            writeEvent(state, event);
        }
    }
}

// -------------------------- main --------------------------

int main(int argc, char* argv[]) {
    libbpf_set_strict_mode(LIBBPF_STRICT_ALL);

    State state{
        .userUid      = getRealUserUid(),
        .pageSize     = static_cast<int>(::getpagesize()),
        .hz           = static_cast<int>(::sysconf(_SC_CLK_TCK)),
        .sampleMs     = 1000,
        .shellPids    = {},
        .processes    = {},
        .mutex        = {},
        .running      = true,
        .outputStream = nullptr,
        .outputMutex  = {},
        .trackMemory  = false,
    };

    runningRef = &state.running;
    std::signal(SIGINT, onSignal);
    std::signal(SIGTERM, onSignal);

    spdlog::set_level(spdlog::level::trace);
    spdlog::set_pattern("[%l] [%g:%#] %v");
    SPDLOG_INFO("Started main execution");

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

    state.trackMemory = j.value("track_memory", false);

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
    if (state.shellPids.empty()) {
        throw std::runtime_error(
            "Could not find any target processes (shells or specified "
            "PIDs)");
    }
    SPDLOG_TRACE("Bootstrap OK");

    int          eventsFd = bpf_map__fd(skel->maps.events);
    ring_buffer* rb       = ring_buffer__new(
        eventsFd, handleEvent, &state, nullptr);
    if (rb == nullptr) {
        throw std::runtime_error{"failed to create ring buffer"};
    }

    std::thread sampler{samplerThread, std::ref(state)};
    SPDLOG_TRACE("Started execution loop");

    while (state.running.load()) {
        int err = ring_buffer__poll(rb, 250);
        if (err < 0) {
            throw std::runtime_error{
                std::format("ring buffer poll failed: {}", err)};
        }
    }

    ring_buffer__free(rb);
    trace_bpf__destroy(skel);
    sampler.join();

    if (state.outputStream) { state.outputStream->close(); }
    SPDLOG_INFO("Closed output stream");

    return 0;
}
