project('elvish_ebpf', ['c', 'cpp'], default_options: ['cpp_std=c++23'])

cc = meson.get_compiler('c')
cpp = meson.get_compiler('cpp')

bpftool = find_program('bpftool', required: true)
clang = find_program('clang', required: true)

spdlog_dep = dependency('spdlog', method: 'pkg-config', required: true)
procps_dep = dependency('libproc2', required: true)
nlohmann_json_dep = dependency('nlohmann_json')

libbpf_dep = cc.find_library('bpf', required: true)
libelf_dep = cc.find_library('elf', required: true)
libz_dep = cc.find_library('z', required: true)

vmlinux_h = custom_target(
  'vmlinux_h',
  output: 'vmlinux.h',
  command: [bpftool, 'btf', 'dump', 'file', '/sys/kernel/btf/vmlinux', 'format', 'c'],
  capture: true
)

bpf_obj = custom_target(
  'trace_bpf_o',
  input: 'trace.bpf.c',
  output: 'trace.bpf.o',
  command: [
    clang,
    '-g',
    '-O2',
    '-target',
    'bpf',
    '-D__TARGET_ARCH_x86',
    '-I', meson.current_build_dir(),
    '-I', '/usr/include',
    '-c', '@INPUT@',
    '-o', '@OUTPUT@'
  ],
  depends: [vmlinux_h]
)

skel_h = custom_target(
  'trace_skel_h',
  input: bpf_obj,
  output: 'trace.skel.h',
  command: [bpftool, 'gen', 'skeleton', '@INPUT@'],
  capture: true
)

executable(
  'elvish_ebpf',
  [meson.project_source_root() / 'main.cpp', skel_h],
  include_directories: include_directories('.'),
  dependencies: [spdlog_dep, libbpf_dep, libelf_dep, libz_dep, procps_dep, nlohmann_json_dep],
  cpp_args: ['-DSPDLOG_ACTIVE_LEVEL=SPDLOG_LEVEL_TRACE'],
  link_args: ['-pthread'],
  install: true,
)
