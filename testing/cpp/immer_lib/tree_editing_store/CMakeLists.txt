cmake_minimum_required(VERSION 3.20)
project(DiagramTreeApp)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(Immer CONFIG REQUIRED)
find_package(Lager CONFIG REQUIRED)
find_package(Zug CONFIG REQUIRED)
find_package(Boost REQUIRED)
find_package(range-v3 REQUIRED)
# cmake -S . -B build -DCMAKE_INSTALL_PREFIX=(pwd)/install -DLOGURU_USE_FMTLIB=1

# cmake --build build

# cmake --install build
set(loguru_DIR "${CMAKE_CURRENT_LIST_DIR}/loguru/install/lib/cmake/loguru")
find_package(fmt REQUIRED)
find_package(loguru REQUIRED)

add_executable(diagram_tree_app main.cpp)

target_link_libraries(diagram_tree_app PRIVATE range-v3::range-v3 immer lager
                                               zug loguru::loguru fmt::fmt)
