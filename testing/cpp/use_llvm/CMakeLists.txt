cmake_minimum_required(VERSION 3.13.4)
project(SimpleProject)

find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")

include_directories(${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})

# Use `add_llvm_...` functions.
# include(AddLLVM)
# Set compiler flags.
# include(HandleLLVMOptions)

# separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
# add_definitions(${LLVM_DEFINITIONS_LIST})


# # Find the libraries that correspond to the LLVM components
# # that we wish to use
# llvm_map_components_to_libnames(llvm_libs support core irreader)


# # Add path to LLVM modules
# set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${LLVM_CMAKE_DIR}")


add_definitions(${LLVM_DEFINITIONS})
add_definitions(${CLANG_DEFINITIONS})

# Now build our tools
add_executable(simple-tool tool.cpp)

# add_library(macro_tracker MODULE macro_expansion_tracker.cpp)
# target_link_libraries(macro_tracker PRIVATE clang-cpp LLVM-15)

# Link against LLVM libraries
target_link_libraries(simple-tool PRIVATE clang-cpp clang LLVM-15)
