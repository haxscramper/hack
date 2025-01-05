cmake_minimum_required(VERSION 3.28)

cmake_policy(SET CMP0135 NEW)
cmake_policy(SET CMP0148 OLD) # or NEW
cmake_policy(SET CMP0155 NEW)
set(CMAKE_CXX_SCAN_FOR_MODULES OFF)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project(org_server)


find_package(Protobuf CONFIG REQUIRED)
find_package(gRPC CONFIG REQUIRED)
find_package(Threads REQUIRED)
find_package(quill REQUIRED)

find_program(GRPC_CPP_PLUGIN grpc_cpp_plugin REQUIRED)

# Generate protobuf and gRPC code
set(PROTO_FILES org.proto)
add_custom_command(
    OUTPUT 
        "${CMAKE_CURRENT_BINARY_DIR}/org.pb.cc"
        "${CMAKE_CURRENT_BINARY_DIR}/org.pb.h"
        "${CMAKE_CURRENT_BINARY_DIR}/org.grpc.pb.cc"
        "${CMAKE_CURRENT_BINARY_DIR}/org.grpc.pb.h"
    COMMAND protobuf::protoc
    ARGS --grpc_out "${CMAKE_CURRENT_BINARY_DIR}"
         --cpp_out "${CMAKE_CURRENT_BINARY_DIR}"
         -I "${CMAKE_CURRENT_SOURCE_DIR}"
         --plugin=protoc-gen-grpc="${GRPC_CPP_PLUGIN}"
         "${CMAKE_CURRENT_SOURCE_DIR}/org.proto"
    DEPENDS ${PROTO_FILES}
)

add_library(org_proto
    ${CMAKE_CURRENT_BINARY_DIR}/org.pb.cc
    ${CMAKE_CURRENT_BINARY_DIR}/org.grpc.pb.cc
    org.hpp
)

target_link_libraries(org_proto
    protobuf::libprotobuf
    gRPC::grpc++
)

target_include_directories(org_proto PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)

add_executable(org_server
    main.cpp
    org_server.cpp
    org_server.hpp
)

target_link_libraries(org_server
    org_proto
    Threads::Threads
    quill::quill
)

target_include_directories(org_server PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)
