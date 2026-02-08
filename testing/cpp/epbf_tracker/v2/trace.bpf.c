#include "vmlinux.h"
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_core_read.h>
#include <bpf/bpf_tracing.h>

struct event {
    __u32 type;
    __u32 pid;
    __u32 ppid;
    __u32 uid;
    __u64 ts;
    __s32 exit_code;
    char comm[16];
};

struct {
    __uint(type, BPF_MAP_TYPE_RINGBUF);
    __uint(max_entries, 1 << 24);
} events SEC(".maps");

SEC("tracepoint/sched/sched_process_exec")
int on_exec(struct trace_event_raw_sched_process_exec* ctx) {
    struct event* e = bpf_ringbuf_reserve(&events, sizeof(struct event), 0);
    if (!e) {
        return 0;
    }

    __u64 id = bpf_get_current_pid_tgid();
    __u32 tgid = id >> 32;

    struct task_struct* task = (struct task_struct*)bpf_get_current_task();
    __u32 ppid = BPF_CORE_READ(task, real_parent, tgid);

    e->type = 0;
    e->pid = tgid;
    e->ppid = ppid;
    e->uid = (__u32)bpf_get_current_uid_gid();
    e->ts = bpf_ktime_get_ns();
    e->exit_code = 0;
    bpf_get_current_comm(&e->comm, sizeof(e->comm));

    bpf_ringbuf_submit(e, 0);
    return 0;
}

SEC("tracepoint/sched/sched_process_exit")
int on_exit(struct trace_event_raw_sched_process_template* ctx) {
    struct event* e = bpf_ringbuf_reserve(&events, sizeof(struct event), 0);
    if (!e) {
        return 0;
    }

    __u64 id = bpf_get_current_pid_tgid();
    __u32 tgid = id >> 32;

    struct task_struct* task = (struct task_struct*)bpf_get_current_task();
    __u32 ppid = BPF_CORE_READ(task, real_parent, tgid);
    __s32 exit_code = BPF_CORE_READ(task, exit_code);

    e->type = 1;
    e->pid = tgid;
    e->ppid = ppid;
    e->uid = (__u32)bpf_get_current_uid_gid();
    e->ts = bpf_ktime_get_ns();
    e->exit_code = exit_code;
    bpf_get_current_comm(&e->comm, sizeof(e->comm));

    bpf_ringbuf_submit(e, 0);
    return 0;
}

char LICENSE[] SEC("license") = "GPL";
