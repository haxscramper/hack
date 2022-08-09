cmake_minimum_required(VERSION 3.5)

project(git_user LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CONAN_DISABLE_CHECK_COMPILER ON)

find_package(
  Boost
  COMPONENTS system filesystem
  REQUIRED)
include(${CMAKE_SOURCE_DIR}/../conanbuildinfo.cmake)
conan_basic_setup()

add_compile_options(-fmax-errors=2)
add_executable(git_user git_user.cpp)

target_link_libraries(git_user matplot)

target_link_libraries(git_user ${Boost_LIBRARIES})
target_link_libraries(git_user ${CONAN_LIBS})

target_link_directories(git_user PRIVATE ${CONAN_LIB_DIRS})
target_link_directories(git_user PRIVATE ${Boost_LIBRARY_DIRS})

target_include_directories(git_user PRIVATE ${Boost_INCLUDE_DIRS})
target_include_directories(git_user PRIVATE ${CONAN_INCLUDE_DIRS})

set_target_properties(git_user PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                                          "${CMAKE_SOURCE_DIR}")
