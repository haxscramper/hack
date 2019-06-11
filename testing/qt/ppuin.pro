#-------------------------------------------------
#
# Project created by QtCreator 2019-05-06T21:03:57
#
#-------------------------------------------------

QT       += core gui xml

CONFIG += console

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

QMAKE_CXXFLAGS += -std=c++17

TARGET = ppuin
TEMPLATE = app

SOURCES += \
        main.cpp \
        mainwindow.cpp \
    assemblyview.cpp \
    assemblyview_arrange.cpp

HEADERS += \
        mainwindow.hpp \
    assemblyview.hpp \
    class_macro.hpp

INCLUDEPATH += \
    external/Support/include \
    external/Algorithm/include \
    external/DebugMacro/include

#include(external/Support/support.pri)
#include(external/DebugMacro/debug.pri)
#include(external/Algorithm/algorithm.pri)
