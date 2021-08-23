QT += gui core

CONFIG += c++11 console
CONFIG -= app_bundle
DEFINES += QT_DEPRECATED_WARNINGS
LIBS *= -lboost_filesystem -lboost_system -pthread -lboost_thread -lboost_wave
DESTDIR = $$PWD

SOURCES += boost_wave.cpp

HEADERS += \
    boost_wave_infile.hpp
