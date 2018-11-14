cmake_minimum_required(VERSION 2.8.3)

find_package(GTest REQUIRED)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")

file(GLOB SOURCES
    ../DebugMacro/debuglogger/*
)

add_executable(
    main
    main.cpp
    ${SOURCES}
    )

target_include_directories(
    main PUBLIC
    ${GTEST_INCLUDE_DIRS}
    ../DebugMacro
    ../Algorithm
    )

target_link_libraries(
    main PUBLIC
    ${GTEST_LIBRARIES}
    )

