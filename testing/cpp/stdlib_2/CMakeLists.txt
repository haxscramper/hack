cmake_minimum_required(VERSION 3.5)

project(stdlib2 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(stdlib2 stdlib_2.cpp)

set_target_properties(stdlib2 PROPERTIES RUNTIME_OUTPUT_DIRECTORY
  "${CMAKE_SOURCE_DIR}")

find_package(Catch2 3 REQUIRED)
target_link_libraries(stdlib2 PRIVATE Catch2::Catch2)
