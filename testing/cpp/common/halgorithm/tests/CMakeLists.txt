cmake_minimum_required(VERSION 3.12)

find_package(GTest REQUIRED)

add_executable(
    test
    main.cpp
    )

set_property(
    TARGET
    test
    PROPERTY
    CXX_STANDARD
    17)

target_include_directories(
    test PUBLIC
    ${GTEST_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}/../Algorithm
    )

target_link_libraries(
    test
    ${GTEST_LIBRARIES}
    )
