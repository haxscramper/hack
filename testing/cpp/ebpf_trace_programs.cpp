#include <bpf/bpf.h>
#include <bpf/libbpf.h>
#include <chrono>
#include <format>
#include <fstream>
#include <linux/bpf.h>
#include <msgpack.hpp>
#include <signal.h>
#include <sys/resource.h>
#include <unistd.h>
#include <unordered_map>

struct process_event {
  __u32 pid;
  __u32 ppid;
  __u64 timestamp;
  __u8 type;
  char comm[16];
};

struct process_info {
  __u32 pid;
  __u32 ppid;
  __u64 start_time;
  __u64 end_time;
  std::string comm;
};

volatile bool running = true;

void signal_handler(int sig) { running = false; }

int handle_event(void *ctx, void *data, size_t data_sz) {
  auto processes = static_cast<std::unordered_map<__u32, process_info> *>(ctx);
  const struct process_event *event =
      static_cast<const struct process_event *>(data);

  if (event->type == 0 || event->type == 1) {
    process_info info{};
    info.pid = event->pid;
    info.ppid = event->ppid;
    info.start_time = event->timestamp;
    info.end_time = 0;
    info.comm = std::string{event->comm};
    processes->insert_or_assign(event->pid, info);
  } else if (event->type == 2) {
    auto it = processes->find(event->pid);
    if (it != processes->end()) {
      it->second.end_time = event->timestamp;
    }
  }

  return 0;
}

int main() {
  struct rlimit rlim{};
  rlim.rlim_cur = rlim.rlim_max = RLIM_INFINITY;
  setrlimit(RLIMIT_MEMLOCK, &rlim);

  signal(SIGINT, signal_handler);
  signal(SIGTERM, signal_handler);

  struct bpf_object *obj = bpf_object__open("process_tracer.o");
  bpf_object__load(obj);

  struct bpf_link *fork_link =
      bpf_program__attach(bpf_object__find_program_by_name(obj, "trace_fork"));
  struct bpf_link *exec_link =
      bpf_program__attach(bpf_object__find_program_by_name(obj, "trace_exec"));
  struct bpf_link *exit_link =
      bpf_program__attach(bpf_object__find_program_by_name(obj, "trace_exit"));

  int map_fd = bpf_object__find_map_fd_by_name(obj, "events");

  std::unordered_map<__u32, process_info> processes{};
  struct ring_buffer *rb =
      ring_buffer__new(map_fd, handle_event, &processes, nullptr);

  while (running) {
    ring_buffer__poll(rb, 100);
  }

  std::vector<std::unordered_map<std::string, msgpack::object>> output_data{};

  for (const auto &[pid, info] : processes) {
    std::unordered_map<std::string, msgpack::object> entry{};
    entry.insert_or_assign("pid", msgpack::object{info.pid});
    entry.insert_or_assign("ppid", msgpack::object{info.ppid});
    entry.insert_or_assign("start_time", msgpack::object{info.start_time});
    entry.insert_or_assign("end_time", msgpack::object{info.end_time});
    entry.insert_or_assign("comm", msgpack::object{info.comm});
    output_data.push_back(entry);
  }

  msgpack::sbuffer buffer{};
  msgpack::pack(buffer, output_data);

  std::ofstream output_file{"process_trace.msgpack", std::ios::binary};
  output_file.write(buffer.data(), buffer.size());
  output_file.close();

  ring_buffer__free(rb);
  bpf_link__destroy(fork_link);
  bpf_link__destroy(exec_link);
  bpf_link__destroy(exit_link);
  bpf_object__close(obj);

  return 0;
}
