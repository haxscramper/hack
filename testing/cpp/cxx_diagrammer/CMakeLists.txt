cmake_minimum_required(VERSION 3.5)

project(cxx_diagrammer LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_COMPILER clang++)
set(CONAN_DISABLE_CHECK_COMPILER ON)

include(${CMAKE_SOURCE_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

add_executable(cxx_diagrammer main.cpp)



add_compile_options(-fcoroutines-ts)

set_target_properties(cxx_diagrammer
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}"
)

target_link_libraries(cxx_diagrammer guile-2.2 gc)
target_link_libraries(cxx_diagrammer CONAN_PKG::fmt)
target_link_libraries(cxx_diagrammer CONAN_PKG::cli11)
target_include_directories(cxx_diagrammer PUBLIC "/usr/include/guile/2.2")

install(TARGETS cxx_diagrammer LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
