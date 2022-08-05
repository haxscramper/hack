QT -= gui

CONFIG += conan_basic_setup
include(conanbuildinfo.pri)

CONFIG += c++2a console
CONFIG -= app_bundle

SOURCES += genwrapper.cpp
LIBS *= -lLLVM -lclang-cpp
QMAKE_CXXFLAGS *= -fpermissive
INCLUDEPATH *= $$PWD/../common
