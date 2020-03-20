QT       += core gui widgets
CONFIG += c++11

DEFINES += QT_DEPRECATED_WARNINGS

SOURCES += \
    main.cpp \
    mainwindow.cpp

HEADERS += \
    mainwindow.hpp

CONFIG += conan_basic_setup
include(conan/conanbuildinfo.pri)


DISTFILES += \
    conanfile.txt
