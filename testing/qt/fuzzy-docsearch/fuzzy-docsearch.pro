QT       += core gui widgets sql
CONFIG += c++11 console

DEFINES += PROJECT_PATH=\"\\\"$$PWD/\\\"\"


SOURCES += \
    main.cpp \
    mainwindow.cpp

HEADERS += \
    base_16_colors.hpp \
    mainwindow.hpp

CONFIG += conan_basic_setup
include(conan/conanbuildinfo.pri)


DISTFILES += \
    conanfile.txt \
    nim.cfg \
    parseast.nim \
    readme.rst
