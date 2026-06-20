cmake_minimum_required(VERSION 3.20)
project(trace_events_sax_parser LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CONAN_PROFILE_PATH "${CMAKE_SOURCE_DIR}/conanprofile.txt")
set(CONAN_OUTPUT_DIR "${CMAKE_SOURCE_DIR}/.conan_install")

find_package(RapidJSON REQUIRED)
find_package(lexy REQUIRED)
find_package(fmt REQUIRED)
find_package(cpptrace REQUIRED)
find_package(quill REQUIRED)
find_package(SQLiteCpp REQUIRED)
find_package(duckdb REQUIRED)
find_package(roaring REQUIRED)
find_package(GTest REQUIRED)
find_package(benchmark REQUIRED)

add_library(
  trace_events_lib
  src/to_json.hpp src/to_json.cpp src/to_json_normalized.hpp
  src/string_interner.hpp src/set_collection.hpp src/set_collection.cpp)

add_executable(trace_event_cli main.cpp)
add_executable(trace_event_test test/tSubsetCollection.cpp
                                test/tStringInterner.cpp)

add_executable(trace_event_bench bench/bSubsetCollection.cpp)

target_include_directories(
  trace_events_lib PUBLIC "${CMAKE_SOURCE_DIR}/cppdecl/include"
                          "${CMAKE_SOURCE_DIR}/src")

target_link_libraries(
  trace_events_lib
  PUBLIC rapidjson
         foonathan::lexy
         fmt::fmt
         cpptrace::cpptrace
         quill::quill
         duckdb::duckdb
         roaring::roaring)

target_link_libraries(trace_event_cli PUBLIC trace_events_lib)

target_link_libraries(trace_event_bench PUBLIC trace_events_lib
                                               benchmark::benchmark_main)

target_link_libraries(trace_event_test PUBLIC trace_events_lib gtest::gtest)
