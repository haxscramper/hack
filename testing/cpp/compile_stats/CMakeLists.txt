cmake_minimum_required(VERSION 3.20)
project(trace_events_sax_parser LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CONAN_PROFILE_PATH "${CMAKE_SOURCE_DIR}/conanprofile.txt")
set(CONAN_OUTPUT_DIR "${CMAKE_SOURCE_DIR}/.conan_install")

execute_process(
  COMMAND
    conan install ${CMAKE_SOURCE_DIR} --output-folder=${CONAN_OUTPUT_DIR}
    -pr:h=${CONAN_PROFILE_PATH} --build=missing -s build_type=Release
    ${CONAN_OPTS}
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  RESULT_VARIABLE CONAN_RESULT
  OUTPUT_VARIABLE CONAN_STDOUT
  ERROR_VARIABLE CONAN_STDERR
  OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE)

message(STATUS "Conan install\n" "---- stdout ----\n${CONAN_STDOUT}\n"
               "---- stderr ----\n${CONAN_STDERR}\n")

if(NOT CONAN_RESULT EQUAL 0)
  message(FATAL_ERROR "Conan install failed (exit code: ${CONAN_RESULT})")
endif()

set(CONAN_TOOLCHAIN
    "${CONAN_OUTPUT_DIR}/build/Release/generators/conan_toolchain.cmake")

include("${CONAN_TOOLCHAIN}")

find_package(RapidJSON REQUIRED)
find_package(lexy REQUIRED)
find_package(fmt REQUIRED)
find_package(cpptrace REQUIRED)
find_package(quill REQUIRED)
find_package(SQLiteCpp REQUIRED)
find_package(duckdb REQUIRED)
find_package(roaring REQUIRED)

add_executable(trace_events_parser main.cpp to_json.hpp to_json.cpp
                                   to_json_normalized.hpp)

target_link_libraries(
  trace_events_parser
  rapidjson
  foonathan::lexy
  fmt::fmt
  cpptrace::cpptrace
  quill::quill
  duckdb::duckdb
  roaring::roaring)

target_include_directories(trace_events_parser
                           PUBLIC "${CMAKE_SOURCE_DIR}/cppdecl/include")
