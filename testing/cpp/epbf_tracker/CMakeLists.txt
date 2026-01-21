# File: CMakeLists.txt
cmake_minimum_required(VERSION 3.30)

project(epbf_tracker LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_C_COMPILER clang)
set(CMAKE_CXX_COMPILER clang++)
set(CMAKE_CXX_SCAN_FOR_MODULES OFF)

find_package(PkgConfig REQUIRED)
pkg_check_modules(LIBBPF REQUIRED libbpf)

find_package(protobuf REQUIRED)

protobuf_generate(LANGUAGE cpp OUT_VAR PROTO_SRCS IMPORT_DIR "${CMAKE_CURRENT_SOURCE_DIR}" PROTOS process_profile.proto PROTOC_OUT_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

set(BPF_OBJECT ${CMAKE_CURRENT_BINARY_DIR}/ebpf_tracker.bpf.o)

set(VMLINUX_H ${CMAKE_CURRENT_BINARY_DIR}/vmlinux.h)

add_custom_command(
  OUTPUT ${VMLINUX_H}
  COMMAND bpftool btf dump file /sys/kernel/btf/vmlinux format c > ${VMLINUX_H}
  COMMENT "Generating vmlinux.h"
  VERBATIM
)

add_custom_command(
  OUTPUT ${BPF_OBJECT}
  COMMAND ${CMAKE_C_COMPILER} -g -O2 -target bpf -fno-stack-protector
          -I${CMAKE_CURRENT_BINARY_DIR}
          -I/usr/include/bpf
          -D__TARGET_ARCH_x86
          -c ${CMAKE_CURRENT_SOURCE_DIR}/ebpf_tracker.bpf.c
          -o ${BPF_OBJECT}
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ebpf_tracker.bpf.c ${VMLINUX_H}
  VERBATIM
)

add_custom_target(ebpf_tracker_bpf ALL DEPENDS ${BPF_OBJECT})

add_executable(epbf_tracker
  main.cpp
  ${PROTO_SRCS}
)

target_compile_definitions(epbf_tracker PRIVATE -DBPF_OBJECT="${BPF_OBJECT}")

add_dependencies(epbf_tracker ebpf_tracker_bpf)

target_include_directories(epbf_tracker PRIVATE
  ${CMAKE_CURRENT_BINARY_DIR}
  ${LIBBPF_INCLUDE_DIRS}
)

target_link_libraries(epbf_tracker PRIVATE
  ${LIBBPF_LIBRARIES}
  protobuf::libprotobuf
)
