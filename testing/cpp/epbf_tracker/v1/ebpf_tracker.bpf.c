#include "vmlinux.h"
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>


enum event_type : __u32 {
  event_exec = 1,
  event_exit = 2,
};

struct event {
  __u32 type;
  __u32 pid;
  __u64 ts_ns;
  char comm[16];
};

struct {
  __uint(type, BPF_MAP_TYPE_RINGBUF);
  __uint(max_entries, 1 << 24);
} events SEC(".maps");

SEC("tracepoint/sched/sched_process_exec")
int on_exec(void *ctx) {
  struct event *e = bpf_ringbuf_reserve(&events, sizeof(struct event), 0);
  if (!e) {
    return 0;
  }

  __u64 pid_tgid = bpf_get_current_pid_tgid();
  e->type = event_exec;
  e->pid = (__u32)(pid_tgid >> 32);
  e->ts_ns = bpf_ktime_get_ns();
  __builtin_memset(e->comm, 0, sizeof(e->comm));
  bpf_get_current_comm(e->comm, sizeof(e->comm));

  bpf_ringbuf_submit(e, 0);
  return 0;
}

SEC("tracepoint/sched/sched_process_exit")
int on_exit(void *ctx) {
  struct event *e = bpf_ringbuf_reserve(&events, sizeof(struct event), 0);
  if (!e) {
    return 0;
  }

  __u64 pid_tgid = bpf_get_current_pid_tgid();
  e->type = event_exit;
  e->pid = (__u32)(pid_tgid >> 32);
  e->ts_ns = bpf_ktime_get_ns();
  __builtin_memset(e->comm, 0, sizeof(e->comm));
  bpf_get_current_comm(e->comm, sizeof(e->comm));

  bpf_ringbuf_submit(e, 0);
  return 0;
}

char LICENSE[] SEC("license") = "GPL";
