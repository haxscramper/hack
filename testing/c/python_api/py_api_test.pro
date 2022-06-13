SOURCES *= main.cpp
CONFIG += c++11
INCLUDEPATH *= /usr/include/python3.10
LIBS *= -lpython3.10

message($$files($$PWD/*.cpp))
