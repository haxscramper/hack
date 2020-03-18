include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()
add_executable(app main.cpp)
target_link_libraries(app ${CONAN_LIBS})
