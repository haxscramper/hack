QT     += core gui widgets
CONFIG += c++17
TARGET  = painter-testbench

CONFIG += conan_basic_setup
include(conan/conanbuildinfo.pri)

SOURCES *= main.cpp
