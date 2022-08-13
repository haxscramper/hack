QT -= gui

CONFIG += conan_basic_setup
include(../conanbuildinfo.pri)


CONFIG += c++2a console
CONFIG -= app_bundle

LIBS *= -lboost_system -lboost_filesystem -lboost_thread -lboost_log
QMAKE_CXXFLAGS *= -fpermissive
SOURCES += git_user.cpp
HEADERS *= ../gitwrap.hpp git_ir.hpp
