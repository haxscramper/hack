project(main)
cmake_minimum_required(VERSION 3.1)
add_executable(main)

execute_process(
  COMMAND nim c --noLinking --nimcache:nim_source_dir "main.nim"
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

file(GLOB nim_sources nim_source_dir/*.c)
message(${CMAKE_CURRENT_SOURCE_DIR})

target_sources(main PUBLIC ${nim_sources})

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR})
find_package(NIM REQUIRED)
target_include_directories(main PUBLIC ${NIM_INCLUDE_DIRS})
