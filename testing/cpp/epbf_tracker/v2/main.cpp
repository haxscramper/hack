#include <bpf/libbpf.h>
#include <chrono>
#include <csignal>
#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <thread>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <spdlog/spdlog.h>

#include "trace.skel.h"

struct Event {
  std::uint32_t type;
  std::uint32_t pid;
  std::uint32_t ppid;
  std::uint32_t uid;
  std::uint64_t ts;
  std::int32_t exit_code;
  char comm[16];
};

struct ProcInfo {
  int pid;
  int ppid;
  int uid;
  int rootShellPid;
  std::uint64_t startNs;
  std::string comm;
  std::string cwd;
  std::vector<std::string> args;
  std::string parentCwd;
  std::vector<std::string> parentArgs;
};

struct State {
  int userUid;
  int pageSize;
  int hz;
  int sampleMs;
  std::unordered_set<int> shellPids;
  std::unordered_map<int, ProcInfo> processes;
  std::mutex mutex;
  std::atomic_bool running;
};

static std::atomic_bool *runningRef = nullptr;

static void onSignal(int sig) {
  if (sig == SIGINT) {
    if (runningRef != nullptr) {
      runningRef->store(false);
    }
  }
  if (sig == SIGTERM) {
    if (runningRef != nullptr) {
      runningRef->store(false);
    }
  }
}

static std::string readFile(const std::filesystem::path &path) {
  std::ifstream input(path, std::ios::in | std::ios::binary);
  if (!input) {
    throw std::runtime_error(std::format("failed to open {}", path.string()));
  }

  std::string content;
  content.assign(std::istreambuf_iterator<char>(input),
                 std::istreambuf_iterator<char>());
  return content;
}

static std::string readComm(int pid) {
  std::ifstream input(std::format("/proc/{}/comm", pid));
  if (!input) {
    throw std::runtime_error(std::format("failed to open /proc/{}/comm", pid));
  }

  std::string line;
  std::getline(input, line); // strips trailing '\n'
  return line;
}

static int readUid(int pid) {
  std::string content =
      readFile(std::filesystem::path{std::format("/proc/{}/status", pid)});
  std::string_view view{content};
  int pos = 0;
  while (pos < static_cast<int>(view.size())) {
    int end = pos;
    while (end < static_cast<int>(view.size()) && view.at(end) != '\n') {
      end += 1;
    }
    std::string_view line = view.substr(pos, end - pos);
    if (line.starts_with("Uid:")) {
      int i = 4;
      while (i < static_cast<int>(line.size()) &&
             (line.at(i) == ' ' || line.at(i) == '\t')) {
        i += 1;
      }
      int j = i;
      while (j < static_cast<int>(line.size()) &&
             ('0' <= line.at(j) && line.at(j) <= '9')) {
        j += 1;
      }
      return std::stoi(std::string{line.substr(i, j - i)});
    }
    pos = end + 1;
  }
  throw std::runtime_error{std::format("uid not found for pid={}", pid)};
}

static int readPpid(int pid) {
  std::string content =
      readFile(std::filesystem::path{std::format("/proc/{}/status", pid)});
  std::string_view view{content};
  int pos = 0;
  while (pos < static_cast<int>(view.size())) {
    int end = pos;
    while (end < static_cast<int>(view.size()) && view.at(end) != '\n') {
      end += 1;
    }
    std::string_view line = view.substr(pos, end - pos);
    if (line.starts_with("PPid:")) {
      int i = 5;
      while (i < static_cast<int>(line.size()) &&
             (line.at(i) == ' ' || line.at(i) == '\t')) {
        i += 1;
      }
      int j = i;
      while (j < static_cast<int>(line.size()) &&
             ('0' <= line.at(j) && line.at(j) <= '9')) {
        j += 1;
      }
      return std::stoi(std::string{line.substr(i, j - i)});
    }
    pos = end + 1;
  }
  throw std::runtime_error{std::format("ppid not found for pid={}", pid)};
}

static std::vector<std::string> readCmdline(int pid) {
  std::string content =
      readFile(std::filesystem::path{std::format("/proc/{}/cmdline", pid)});
  std::vector<std::string> args{};
  std::string current{};
  for (int i = 0; i < static_cast<int>(content.size()); i += 1) {
    char c = content.at(i);
    if (c == '\0') {
      if (0 < static_cast<int>(current.size())) {
        args.push_back(current);
        current.clear();
      }
    } else {
      current.push_back(c);
    }
  }
  if (0 < static_cast<int>(current.size())) {
    args.push_back(current);
  }
  return args;
}

static std::string readCwd(int pid) {
  std::filesystem::path link{std::format("/proc/{}/cwd", pid)};
  std::filesystem::path target = std::filesystem::read_symlink(link);
  return target.string();
}

static std::uint64_t readStartNsFromProcStat(int pid, int hz) {
  std::string content =
      readFile(std::filesystem::path{std::format("/proc/{}/stat", pid)});
  int closeParen = -1;
  for (int i = 0; i < static_cast<int>(content.size()); i += 1) {
    if (content.at(i) == ')') {
      closeParen = i;
    }
  }
  if (closeParen < 0) {
    throw std::runtime_error{std::format("invalid /proc/{}/stat", pid)};
  }
  int start = closeParen + 2;
  if (static_cast<int>(content.size()) < start) {
    throw std::runtime_error{std::format("invalid /proc/{}/stat", pid)};
  }
  std::string_view tail{content.data() + start, content.size() - start};
  std::vector<std::string_view> tokens{};
  int pos = 0;
  while (pos < static_cast<int>(tail.size())) {
    while (pos < static_cast<int>(tail.size()) && tail.at(pos) == ' ') {
      pos += 1;
    }
    int end = pos;
    while (end < static_cast<int>(tail.size()) && tail.at(end) != ' ') {
      end += 1;
    }
    if (pos < end) {
      tokens.push_back(tail.substr(pos, end - pos));
    }
    pos = end;
  }
  if (static_cast<int>(tokens.size()) <= 19) {
    throw std::runtime_error{std::format("invalid /proc/{}/stat token count={}",
                                         pid, static_cast<int>(tokens.size()))};
  }
  std::uint64_t ticks =
      static_cast<std::uint64_t>(std::stoull(std::string{tokens.at(19)}));
  std::uint64_t ns = (ticks * 1000000000ULL) / static_cast<std::uint64_t>(hz);
  return ns;
}

static std::uint64_t readRssKb(int pid, int pageSize) {
  std::string content =
      readFile(std::filesystem::path{std::format("/proc/{}/statm", pid)});
  std::string_view view{content};
  int pos = 0;
  while (pos < static_cast<int>(view.size()) && view.at(pos) != ' ') {
    pos += 1;
  }
  while (pos < static_cast<int>(view.size()) && view.at(pos) == ' ') {
    pos += 1;
  }
  int end = pos;
  while (end < static_cast<int>(view.size()) && view.at(end) != ' ' &&
         view.at(end) != '\n') {
    end += 1;
  }
  std::uint64_t residentPages = static_cast<std::uint64_t>(
      std::stoull(std::string{view.substr(pos, end - pos)}));
  std::uint64_t rssBytes = residentPages * static_cast<std::uint64_t>(pageSize);
  std::uint64_t rssKb = rssBytes / 1024ULL;
  return rssKb;
}

static std::string joinArgs(std::vector<std::string> const &args) {
  std::string out{};
  for (int i = 0; i < static_cast<int>(args.size()); i += 1) {
    if (0 < i) {
      out.append(" ");
    }
    out.append(args.at(i));
  }
  return out;
}

static void logStart(ProcInfo const &info) {
  std::string argv = joinArgs(info.args);
  std::string parentArgv = joinArgs(info.parentArgs);
  SPDLOG_INFO("{}",
              std::format("start pid={} ppid={} root={} comm={} cwd={} "
                          "argv={} parent_cwd={} parent_argv={}",
                          info.pid, info.ppid, info.rootShellPid, info.comm,
                          info.cwd, argv, info.parentCwd, parentArgv));
}

static void logStop(ProcInfo const &info, std::uint64_t stopNs,
                    std::int32_t exitCode) {
  double seconds = static_cast<double>(stopNs - info.startNs) / 1000000000.0;
  SPDLOG_INFO("{}",
              std::format("stop pid={} comm={} seconds={:.6f} exit_code={}",
                          info.pid, info.comm, seconds, exitCode));
}

static void bootstrap(State &state) {
  std::unordered_map<int, int> pidToPpid{};
  std::unordered_map<int, int> pidToUid{};
  std::unordered_map<int, std::string> pidToComm{};

  for (std::filesystem::directory_entry const &entry :
       std::filesystem::directory_iterator{"/proc"}) {
    if (!entry.is_directory()) {
      continue;
    }
    std::string name = entry.path().filename().string();
    bool isNumber = true;
    for (int i = 0; i < static_cast<int>(name.size()); i += 1) {
      char c = name.at(i);
      if (!(('0' <= c && c <= '9'))) {
        isNumber = false;
      }
    }
    if (!isNumber) {
      continue;
    }
    int pid = std::stoi(name);
    if (!(0 < pid)) {
      continue;
    }
    int uid = readUid(pid);
    std::string comm = readComm(pid);
    if (comm == "elvish") {
      SPDLOG_DEBUG("Found elvish pid={} uid={} userUid={}", pid, uid,
                   state.userUid);
    }

    if (uid != state.userUid) {
      continue;
    }

    int ppid = readPpid(pid);
    pidToPpid.insert_or_assign(pid, ppid);
    pidToUid.insert_or_assign(pid, uid);
    pidToComm.insert_or_assign(pid, comm);
    if (comm == "elvish") {
      state.shellPids.insert(pid);
    }
  }

  for (std::filesystem::directory_entry const &entry :
       std::filesystem::directory_iterator{"/proc"}) {
    if (!entry.is_directory()) {
      continue;
    }
    std::string name = entry.path().filename().string();
    bool isNumber = true;
    for (int i = 0; i < static_cast<int>(name.size()); i += 1) {
      char c = name.at(i);
      if (!(('0' <= c && c <= '9'))) {
        isNumber = false;
      }
    }
    if (!isNumber) {
      continue;
    }
    int pid = std::stoi(name);
    if (!(0 < pid)) {
      continue;
    }
    if (!pidToUid.contains(pid)) {
      continue;
    }
    if (state.shellPids.contains(pid)) {
      continue;
    }

    int ppid = pidToPpid.at(pid);
    int current = ppid;
    int root = 0;
    while (0 < current) {
      if (state.shellPids.contains(current)) {
        root = current;
        current = 0;
      } else {
        if (!pidToPpid.contains(current)) {
          current = 0;
        } else {
          current = pidToPpid.at(current);
        }
      }
    }
    if (!(0 < root)) {
      continue;
    }

    std::uint64_t startNs = readStartNsFromProcStat(pid, state.hz);
    std::string comm = pidToComm.at(pid);
    std::string cwd = readCwd(pid);
    std::vector<std::string> args = readCmdline(pid);
    std::string parentCwd = readCwd(ppid);
    std::vector<std::string> parentArgs = readCmdline(ppid);

    ProcInfo info{
        .pid = pid,
        .ppid = ppid,
        .uid = state.userUid,
        .rootShellPid = root,
        .startNs = startNs,
        .comm = comm,
        .cwd = cwd,
        .args = args,
        .parentCwd = parentCwd,
        .parentArgs = parentArgs,
    };

    state.processes.insert_or_assign(pid, info);
    logStart(state.processes.at(pid));
  }
}

static int handleEvent(void *ctx, void *data, size_t dataSize) {
  if (dataSize < sizeof(Event)) {
    throw std::runtime_error{"event size mismatch"};
  }
  State &state = *static_cast<State *>(ctx);
  Event const &e = *static_cast<Event const *>(data);

  int pid = static_cast<int>(e.pid);
  int ppid = static_cast<int>(e.ppid);
  int uid = static_cast<int>(e.uid);

  if (uid != state.userUid) {
    return 0;
  }

  std::string comm = std::string{e.comm};
  if (e.type == 0) {
    std::scoped_lock<std::mutex> lock{state.mutex};

    if (comm == "elvish") {
      state.shellPids.insert(pid);
      return 0;
    }

    int root = 0;
    if (state.shellPids.contains(ppid)) {
      root = ppid;
    } else {
      if (state.processes.contains(ppid)) {
        root = state.processes.at(ppid).rootShellPid;
      }
    }

    if (!(0 < root)) {
      return 0;
    }

    std::string cwd = readCwd(pid);
    std::vector<std::string> args = readCmdline(pid);
    std::string parentCwd = readCwd(ppid);
    std::vector<std::string> parentArgs = readCmdline(ppid);

    ProcInfo info{
        .pid = pid,
        .ppid = ppid,
        .uid = uid,
        .rootShellPid = root,
        .startNs = static_cast<std::uint64_t>(e.ts),
        .comm = comm,
        .cwd = cwd,
        .args = args,
        .parentCwd = parentCwd,
        .parentArgs = parentArgs,
    };

    state.processes.insert_or_assign(pid, info);
    logStart(state.processes.at(pid));
    return 0;
  }

  if (e.type == 1) {
    std::scoped_lock<std::mutex> lock{state.mutex};
    if (!state.processes.contains(pid)) {
      return 0;
    }
    ProcInfo info = state.processes.at(pid);
    logStop(info, static_cast<std::uint64_t>(e.ts), e.exit_code);
    state.processes.erase(pid);
    return 0;
  }

  return 0;
}

static void samplerThread(State &state) {
  while (state.running.load()) {
    std::this_thread::sleep_for(std::chrono::milliseconds{state.sampleMs});
    std::vector<int> pids{};
    {
      std::scoped_lock<std::mutex> lock{state.mutex};
      pids.reserve(static_cast<int>(state.processes.size()));
      for (std::pair<int const, ProcInfo> const &kv : state.processes) {
        pids.push_back(kv.first);
      }
    }
    for (int i = 0; i < static_cast<int>(pids.size()); i += 1) {
      int pid = pids.at(i);
      std::uint64_t rssKb = readRssKb(pid, state.pageSize);
      std::scoped_lock<std::mutex> lock{state.mutex};
      if (state.processes.contains(pid)) {
        ProcInfo const &info = state.processes.at(pid);
        SPDLOG_INFO("{}", std::format("mem pid={} comm={} rss_kb={}", pid,
                                      info.comm, rssKb));
      }
    }
  }
}

int main() {
  libbpf_set_strict_mode(LIBBPF_STRICT_ALL);

  State state{
      .userUid = static_cast<int>(::getuid()),
      .pageSize = static_cast<int>(::getpagesize()),
      .hz = static_cast<int>(::sysconf(_SC_CLK_TCK)),
      .sampleMs = 1000,
      .shellPids = {},
      .processes = {},
      .mutex = {},
      .running = true,
  };

  runningRef = &state.running;
  std::signal(SIGINT, onSignal);
  std::signal(SIGTERM, onSignal);

  spdlog::set_level(spdlog::level::trace);
  spdlog::set_pattern("[%l] [%g:%#] %v");
  SPDLOG_INFO("Started main execution");

  trace_bpf *skel = trace_bpf__open_and_load();
  if (skel == nullptr) {
    throw std::runtime_error{"failed to open and load bpf skeleton"};
  }
  int attachErr = trace_bpf__attach(skel);
  if (attachErr < 0) {
    throw std::runtime_error{
        std::format("failed to attach bpf programs: {}", attachErr)};
  }

  bootstrap(state);
  if (state.shellPids.size() == 0) {
    throw std::runtime_error(
        "Could not find any running `elvish` shell instances");
  }
  SPDLOG_TRACE("Bootstrap OK");

  int eventsFd = bpf_map__fd(skel->maps.events);
  ring_buffer *rb = ring_buffer__new(eventsFd, handleEvent, &state, nullptr);
  if (rb == nullptr) {
    throw std::runtime_error{"failed to create ring buffer"};
  }

  std::thread sampler{samplerThread, std::ref(state)};
  SPDLOG_TRACE("Started execution loop");

  while (state.running.load()) {
    int err = ring_buffer__poll(rb, 250);
    if (err < 0) {
      throw std::runtime_error{std::format("ring buffer poll failed: {}", err)};
    }
  }

  ring_buffer__free(rb);
  trace_bpf__destroy(skel);
  sampler.join();
  return 0;
}
