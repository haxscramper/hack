cmake_minimum_required(VERSION 3.5)

project(stdlib2 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(stdlib2 stdlib_2.cpp)

if(${CMAKE_CXX_COMPILER_ID} MATCHES Clang)
  message(INFO " Using compiler 'clang++'")
  target_compile_options(stdlib2 PRIVATE -ferror-limit=1)
  if(${TEST_COVERAGE} STREQUAL "ON")
    message(INFO " Test coverage enabled")
    target_compile_options(
      stdlib2 PRIVATE -fprofile-instr-generate -fcoverage-mapping
                      -ftest-coverage)
    target_link_options(stdlib2 PRIVATE -lgcov --coverage
                        -fprofile-instr-generate -fcoverage-mapping)

  endif()
endif()

if(${CMAKE_CXX_COMPILER_ID} MATCHES GNU)
  message(INFO " Using compiler 'g++'")
  target_compile_options(stdlib2 PRIVATE -fmax-errors=1)
  if(${TEST_COVERAGE} STREQUAL "ON")
    message(INFO " Test coverage enabled")
    target_compile_options(stdlib2 PRIVATE --coverage)
    target_link_options(stdlib2 PRIVATE -lgcov --coverage)
  endif()
endif()

# target_compile_options(stdlib2 PRIVATE # -fdiagnostics-format=json)

set_target_properties(stdlib2 PROPERTIES OUTPUT_NAME stdlib2 SUFFIX ".bin")
set_target_properties(stdlib2 PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                                         "${CMAKE_SOURCE_DIR}")

find_package(Catch2 3 REQUIRED)
target_link_libraries(stdlib2 PRIVATE Catch2::Catch2)
