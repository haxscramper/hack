cmake_minimum_required(VERSION 3.28)

cmake_policy(SET CMP0135 NEW)
cmake_policy(SET CMP0148 OLD) # or NEW
cmake_policy(SET CMP0155 NEW)
set(CMAKE_CXX_SCAN_FOR_MODULES OFF)

project(feature_testing LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 23)

add_executable(
  main
  code_patterns.cpp
  matplot_1.cpp
  mutex_simple.cpp
  pass_lambda_to_git_api.cpp
  std_async_example.cpp
  std_binary_semaphore.cpp
  virtual_testing.cpp
  util.cpp
  util.hpp
  main.cpp)

find_package(Matplot++ REQUIRED)
find_package(GTest CONFIG REQUIRED)
find_package(quill REQUIRED)

find_package(PkgConfig)
pkg_check_modules(LIBGIT2 REQUIRED libgit2)

target_link_libraries(main PUBLIC "${LIBGIT2_LIBRARIES}")
target_include_directories(main PUBLIC "${LIBGIT2_INCLUDE_DIRS}")

target_link_libraries(main PUBLIC GTest::gtest Matplot++::matplot quill::quill)
