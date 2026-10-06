cmake_minimum_required(VERSION 3.20)

project(activity-directories LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_AUTOMOC ON)

find_package(ECM REQUIRED NO_MODULE)
set(CMAKE_MODULE_PATH ${ECM_MODULE_PATH})

find_package(Qt6 REQUIRED COMPONENTS Core)
find_package(PlasmaActivities REQUIRED)
find_package(PlasmaActivitiesStats REQUIRED)

add_executable(activity-directories main.cpp)

target_link_libraries(activity-directories PRIVATE Qt6::Core Plasma::Activities
                                                   Plasma::ActivitiesStats)
