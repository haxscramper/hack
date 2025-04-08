# CMakeLists.txt
cmake_minimum_required(VERSION 3.14)
project(myaddon)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# find_package(Nodejs REQUIRED) find_package(NAPI REQUIRED)

add_library(myaddon MODULE module.cpp)
target_include_directories(myaddon PRIVATE "node_modules/node-addon-api"
                                           "/usr/include/node")
# target_link_libraries(myaddon PRIVATE ${NAPI_LIBRARIES})

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/build)
set_target_properties(
  myaddon
  PROPERTIES PREFIX ""
             SUFFIX ".node"
             OUTPUT_NAME "myaddon")
