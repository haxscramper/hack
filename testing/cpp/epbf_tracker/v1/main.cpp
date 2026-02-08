// File: src/main.cpp
#include "process_profile.pb.h"

#include <bpf/libbpf.h>

#include <cerrno>
#include <chrono>
#include <cstdint>
#include <cstring>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <thread>
#include <unordered_map>
#include <utility>
#include <vector>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include <google/protobuf/io/coded_stream.h>
#include <google/protobuf/io/zero_copy_stream_impl.h>

#include "debug.hpp"

#ifndef BPF_OBJECT
#error "BPF_OBJECT" path must be provided at the build time
#endif

namespace fs = std::filesystem;

struct BpfEvent {
  std::uint32_t type;
  std::uint32_t pid;
  std::uint64_t ts_ns;
  char comm[16];
};

struct ProcessSnapshot {
  std::uint32_t pid;
  std::uint32_t ppid;
  std::uint64_t start_ns;
  std::string comm;
  std::string cwd;
  std::string cmdline;
};

struct Writer {
  std::mutex mutex;
  std::ofstream stream;

  explicit Writer(std::string const &path) : stream{} {
    try {
      auto parent_path = std::filesystem::path(path).parent_path();
      if (!parent_path.empty()) {
        std::filesystem::create_directories(parent_path);
      }

      stream.open(path, std::ios::binary | std::ios::trunc | std::ios::out);

      if (!stream.is_open()) {
        throw std::runtime_error{
            std::format("failed to open output file: {} (errno: {})", path,
                        strerror(errno))};
      }
    } catch (const std::filesystem::filesystem_error &ex) {
      throw std::runtime_error{
          std::format("filesystem error for path {}: {}", path, ex.what())};
    }
  }

  void writeDelimited(google::protobuf::Message const &message) {
    std::lock_guard<std::mutex> lock{mutex};

    google::protobuf::io::OstreamOutputStream zeroCopy{&stream};
    google::protobuf::io::CodedOutputStream coded{&zeroCopy};

    std::size_t const size = static_cast<std::size_t>(message.ByteSizeLong());
    coded.WriteVarint32(static_cast<std::uint32_t>(size));
    if (!message.SerializeToCodedStream(&coded)) {
      throw std::runtime_error{"failed to serialize protobuf message"};
    }
    stream.flush();
  }
};

enum class TrackerMode { WAITING_FOR_ROOT, TRACKING };

struct TrackerState {
  std::mutex mutex;

  std::uint32_t rootPid{0U};
  bool rootExited{false};
  TrackerMode mode{TrackerMode::WAITING_FOR_ROOT};

  std::unordered_map<std::uint32_t, bool> trackedPids{};
  std::unordered_map<std::uint32_t, ProcessSnapshot> processes{};
};

static std::uint64_t monotonicNs() {
  timespec ts{};
  int const rc = ::clock_gettime(CLOCK_MONOTONIC, &ts);
  if (rc != 0) {
    return 0ULL;
  }
  return static_cast<std::uint64_t>(ts.tv_sec) * 1'000'000'000ULL +
         static_cast<std::uint64_t>(ts.tv_nsec);
}

static std::optional<std::string> readSmallFile(fs::path const &path) {
  std::ifstream f{path, std::ios::binary};
  if (!f.is_open()) {
    return std::nullopt;
  }

  std::string data{};
  f.seekg(0, std::ios::end);
  std::streamoff const size = f.tellg();
  if (size < 0) {
    return std::nullopt;
  }
  f.seekg(0, std::ios::beg);

  data.resize(static_cast<std::size_t>(size));
  f.read(data.data(), static_cast<std::streamsize>(data.size()));
  if (!f) {
    return std::nullopt;
  }

  return data;
}

static std::optional<std::string> readCmdline(std::uint32_t pid) {
  fs::path const path = fs::path{"/proc"} / std::to_string(pid) / "cmdline";
  std::optional<std::string> dataOpt = readSmallFile(path);
  if (!dataOpt.has_value()) {
    return std::nullopt;
  }

  std::string const &data = dataOpt.value();
  if (data.empty()) {
    return std::string{};
  }

  std::string out{};
  out.reserve(data.size());

  bool first{true};
  for (char const ch : data) {
    if (ch == '\0') {
      if (!first) {
        out.push_back(' ');
      }
      first = false;
      continue;
    }
    out.push_back(ch);
  }

  while (!out.empty() && out.back() == ' ') {
    out.pop_back();
  }

  return out;
}

static std::optional<std::string> readCwd(std::uint32_t pid) {
  fs::path const path = fs::path{"/proc"} / std::to_string(pid) / "cwd";
  std::vector<char> buf{};
  buf.resize(4096);

  ssize_t const n = ::readlink(path.c_str(), buf.data(), buf.size());
  if (n < 0) {
    return std::nullopt;
  }

  return std::string{buf.data(), static_cast<std::size_t>(n)};
}

static std::optional<std::uint32_t> readPpid(std::uint32_t pid) {
  fs::path const path = fs::path{"/proc"} / std::to_string(pid) / "stat";
  std::optional<std::string> dataOpt = readSmallFile(path);
  if (!dataOpt.has_value()) {
    return std::nullopt;
  }

  std::string const &s = dataOpt.value();
  std::size_t const rparen = s.rfind(')');
  if (rparen == std::string::npos) {
    return std::nullopt;
  }

  std::size_t const after = rparen + 1;
  if (s.size() <= after) {
    return std::nullopt;
  }

  std::string_view const tail{s.data() + after, s.size() - after};

  std::size_t pos{0};
  while (pos < tail.size() && (tail.at(pos) == ' ' || tail.at(pos) == '\t')) {
    pos += 1;
  }
  if (tail.size() <= pos) {
    return std::nullopt;
  }

  while (pos < tail.size() && tail.at(pos) != ' ' && tail.at(pos) != '\t') {
    pos += 1;
  }
  while (pos < tail.size() && (tail.at(pos) == ' ' || tail.at(pos) == '\t')) {
    pos += 1;
  }

  std::size_t ppidStart = pos;
  while (pos < tail.size() && tail.at(pos) != ' ' && tail.at(pos) != '\t') {
    pos += 1;
  }
  if (!(ppidStart < pos)) {
    return std::nullopt;
  }

  std::string const ppidStr{tail.substr(ppidStart, pos - ppidStart)};
  try {
    unsigned long const ppidUl = std::stoul(ppidStr);
    return static_cast<std::uint32_t>(ppidUl);
  } catch (...) {
    return std::nullopt;
  }
}

static std::optional<std::uint64_t> readRssBytes(std::uint32_t pid) {
  fs::path const path = fs::path{"/proc"} / std::to_string(pid) / "statm";
  std::optional<std::string> dataOpt = readSmallFile(path);
  if (!dataOpt.has_value()) {
    return std::nullopt;
  }

  std::string const &s = dataOpt.value();
  std::size_t pos{0};

  while (pos < s.size() && s.at(pos) == ' ') {
    pos += 1;
  }
  while (pos < s.size() && s.at(pos) != ' ') {
    pos += 1;
  }
  while (pos < s.size() && s.at(pos) == ' ') {
    pos += 1;
  }

  std::size_t start = pos;
  while (pos < s.size() && s.at(pos) != ' ' && s.at(pos) != '\n') {
    pos += 1;
  }
  if (!(start < pos)) {
    return std::nullopt;
  }

  std::string const residentStr = s.substr(start, pos - start);
  unsigned long residentPages{0UL};
  try {
    residentPages = std::stoul(residentStr);
  } catch (...) {
    return std::nullopt;
  }

  long const pageSize = ::sysconf(_SC_PAGESIZE);
  if (pageSize < 1) {
    return std::nullopt;
  }

  return static_cast<std::uint64_t>(residentPages) *
         static_cast<std::uint64_t>(pageSize);
}

static bool matchesRootPattern(std::string const &cmdline) {
  bool const hasCmake = cmdline.find("cmake") != std::string::npos;
  bool const hasBuild = cmdline.find("--build") != std::string::npos;
  return hasCmake && hasBuild;
}

struct Context {
  Writer *writer;
  TrackerState *state;
};

static void emitStarted(Writer &writer, ProcessSnapshot const &snapshot) {
  epbf_tracker::ProcessEvent event{};
  epbf_tracker::ProcessStarted *started = event.mutable_started();

  started->set_pid(snapshot.pid);
  started->set_ppid(snapshot.ppid);
  started->set_start_ns(snapshot.start_ns);
  started->set_comm(snapshot.comm);
  started->set_cwd(snapshot.cwd);
  started->set_cmdline(snapshot.cmdline);

  writer.writeDelimited(event);
}

static void emitEnded(Writer &writer, std::uint32_t pid, std::uint64_t endNs) {
  epbf_tracker::ProcessEvent event{};
  epbf_tracker::ProcessEnded *ended = event.mutable_ended();

  ended->set_pid(pid);
  ended->set_end_ns(endNs);

  writer.writeDelimited(event);
}

static void emitMemory(Writer &writer, std::uint32_t pid, std::uint64_t tsNs,
                       std::uint64_t rssBytes) {
  epbf_tracker::ProcessEvent event{};
  epbf_tracker::ProcessMemory *mem = event.mutable_memory();

  mem->set_pid(pid);
  mem->set_ts_ns(tsNs);
  mem->set_rss_bytes(rssBytes);

  writer.writeDelimited(event);
}

static int handleRingEvent(void *ctxVoid, void *data, std::size_t size) {
  if (size < sizeof(BpfEvent)) {
    return 0;
  }

  Context &ctx = *static_cast<Context *>(ctxVoid);
  Writer &writer = *ctx.writer;
  TrackerState &state = *ctx.state;

  BpfEvent const &ev = *static_cast<BpfEvent const *>(data);
  INFO("Handle ring event trigger event type '{}'", ev.type);

  if (ev.type == 1U) {
    std::uint32_t const pid = ev.pid;

    std::optional<std::uint32_t> ppidOpt = readPpid(pid);
    std::optional<std::string> cwdOpt = readCwd(pid);
    std::optional<std::string> cmdlineOpt = readCmdline(pid);

    std::uint32_t const ppid = ppidOpt.has_value() ? ppidOpt.value() : 0U;
    std::string const cwd = cwdOpt.has_value() ? cwdOpt.value() : std::string{};
    std::string const cmdline =
        cmdlineOpt.has_value() ? cmdlineOpt.value() : std::string{};
    std::string const comm =
        std::string{ev.comm, strnlen(ev.comm, sizeof(ev.comm))};

    ProcessSnapshot const snapshot{
        .pid = pid,
        .ppid = ppid,
        .start_ns = ev.ts_ns,
        .comm = comm,
        .cwd = cwd,
        .cmdline = cmdline,
    };

    bool shouldTrack{false};
    {
      std::lock_guard<std::mutex> lock{state.mutex};

      if (state.mode == TrackerMode::WAITING_FOR_ROOT) {
        if (matchesRootPattern(snapshot.cmdline)) {
          INFO("Started tracking root process {}", pid);
          state.mode = TrackerMode::TRACKING;
          state.rootPid = pid;
          state.trackedPids.insert_or_assign(pid, true);
          state.processes.insert_or_assign(pid, snapshot);
          shouldTrack = true;
        }
        // Ignore all other processes while waiting for root
      } else if (state.mode == TrackerMode::TRACKING) {
        if (state.rootPid == 0U) {
          if (matchesRootPattern(snapshot.cmdline)) {
            state.rootPid = pid;
            state.trackedPids.insert_or_assign(pid, true);
            state.processes.insert_or_assign(pid, snapshot);
            shouldTrack = true;
          }
        } else {
          auto const itParent = state.trackedPids.find(ppid);
          if (itParent != state.trackedPids.end()) {
            state.trackedPids.insert_or_assign(pid, true);
            state.processes.insert_or_assign(pid, snapshot);
            shouldTrack = true;
          }
        }
      }
    }

    if (shouldTrack) {
      INFO("Started process {} '{}'", pid, cmdline);
      emitStarted(writer, snapshot);
    }
  }

  if (ev.type == 2U) {
    std::uint32_t const pid = ev.pid;

    bool shouldEmit{false};
    bool done{false};
    {
      std::lock_guard<std::mutex> lock{state.mutex};

      // Only handle exits if we're in tracking mode
      if (state.mode == TrackerMode::TRACKING) {
        auto const it = state.trackedPids.find(pid);
        if (it != state.trackedPids.end()) {
          shouldEmit = true;
          state.trackedPids.erase(pid);
          state.processes.erase(pid);
        }

        if (pid == state.rootPid) {
          INFO("Root process exited");
          state.rootExited = true;
        }

        if (state.rootExited && state.trackedPids.empty()) {
          INFO("No more PIDs to track, exiting the process");
          done = true;
        }
      }
    }

    if (shouldEmit) {
      emitEnded(writer, pid, ev.ts_ns);
    }

    if (done) {
      INFO("Done processing, returning 1");
      return 1;
    }
  }

  return 0;
}

static void memorySampler(Writer &writer, TrackerState &state,
                          std::atomic_bool &stopFlag) {
  while (!stopFlag.load()) {
    std::vector<std::uint32_t> pids{};
    bool isTracking{false};

    {
      std::lock_guard<std::mutex> lock{state.mutex};
      isTracking = (state.mode == TrackerMode::TRACKING);
      if (isTracking) {
        pids.reserve(state.trackedPids.size());
        for (auto const &kv : state.trackedPids) {
          pids.insert(pids.end(), kv.first);
        }
      }
    }

    // Only sample memory if we're in tracking mode and have processes to track
    if (isTracking && !pids.empty()) {
      std::uint64_t const tsNs = monotonicNs();

      for (std::uint32_t const pid : pids) {
        std::optional<std::uint64_t> rssOpt = readRssBytes(pid);
        if (!rssOpt.has_value()) {
          continue;
        }

        bool stillTracked{false};
        {
          std::lock_guard<std::mutex> lock{state.mutex};
          auto const it = state.trackedPids.find(pid);
          stillTracked = it != state.trackedPids.end();
        }
        if (!stillTracked) {
          continue;
        }

        emitMemory(writer, pid, tsNs, rssOpt.value());
      }
    }

    std::this_thread::sleep_for(std::chrono::milliseconds{250});
  }
}

static void throwLibbpfError(std::string const &what, int err) {
  if (err < 0) {
    int const e = -err;
    throw std::runtime_error{
        std::format("{}: {} ({})", what, std::strerror(e), e)};
  }
  if (0 < err) {
    throw std::runtime_error{std::format("{}: {}", what, err)};
  }
}

int main() {
  INFO("Logger setup");

  ::libbpf_set_strict_mode(LIBBPF_STRICT_ALL);

  fs::path const bpfObjectPath = BPF_OBJECT;
  if (!fs::exists(bpfObjectPath)) {
    throw std::runtime_error{
        std::format("missing bpf object: {}", bpfObjectPath.string())};
  }

  bpf_object *obj = ::bpf_object__open_file(bpfObjectPath.c_str(), nullptr);
  if (obj == nullptr) {
    throw std::runtime_error{"bpf_object__open_file failed"};
  }

  int err = ::bpf_object__load(obj);
  throwLibbpfError("bpf_object__load failed", err);

  bpf_program *execProg = ::bpf_object__find_program_by_name(obj, "on_exec");
  if (execProg == nullptr) {
    throw std::runtime_error{"failed to find bpf program on_exec"};
  }

  bpf_program *exitProg = ::bpf_object__find_program_by_name(obj, "on_exit");
  if (exitProg == nullptr) {
    throw std::runtime_error{"failed to find bpf program on_exit"};
  }

  bpf_link *execLink =
      ::bpf_program__attach_tracepoint(execProg, "sched", "sched_process_exec");
  if (execLink == nullptr) {
    throw std::runtime_error{"failed to attach sched:sched_process_exec"};
  }

  bpf_link *exitLink =
      ::bpf_program__attach_tracepoint(exitProg, "sched", "sched_process_exit");
  if (exitLink == nullptr) {
    throw std::runtime_error{"failed to attach sched:sched_process_exit"};
  }

  bpf_map *eventsMap = ::bpf_object__find_map_by_name(obj, "events");
  if (eventsMap == nullptr) {
    throw std::runtime_error{"failed to find ringbuf map 'events'"};
  }

  int const eventsFd = ::bpf_map__fd(eventsMap);
  if (eventsFd < 0) {
    throw std::runtime_error{"failed to get ringbuf fd"};
  }

  INFO("Started writer");
  Writer writer{"ebpf_process_profile.bin"};
  TrackerState state{};
  Context cbCtx{.writer = &writer, .state = &state};

  ring_buffer *rb =
      ::ring_buffer__new(eventsFd, handleRingEvent, &cbCtx, nullptr);
  if (rb == nullptr) {
    throw std::runtime_error{"ring_buffer__new failed"};
  }

  INFO("Started memory thread");
  std::atomic_bool stopFlag{false};
  std::thread memThread{[&writer, &state, &stopFlag]() {
    memorySampler(writer, state, stopFlag);
  }};

  bool done{false};
  while (!done) {
    int const rc = ::ring_buffer__poll(rb, 200);
    if (rc < 0) {
      int const e = -rc;
      throw std::runtime_error{
          std::format("ring_buffer__poll: {} ({})", std::strerror(e), e)};
    }
    if (rc == 1) {
      INFO("Ring buffer poll result is 1, process done");
      done = true;
    }
  }

  stopFlag.store(true);
  memThread.join();

  ::ring_buffer__free(rb);
  ::bpf_link__destroy(execLink);
  ::bpf_link__destroy(exitLink);
  ::bpf_object__close(obj);

  return 0;
}
