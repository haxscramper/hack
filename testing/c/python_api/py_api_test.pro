SOURCES *= main.cpp

HEADERS *= \
    subinterpreters_from_tests.hpp \
    with_qt.hpp \
    with_subinterpreters.hpp

CONFIG += c++11
QT += core
INCLUDEPATH *= /usr/include/python3.10
LIBS *= -lpython3.10

message($$files($$PWD/*.cpp))
