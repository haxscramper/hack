QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

CONFIG += c++17
DEFINES += QT_DEPRECATED_WARNINGS
SOURCES += main.cpp



LIBS += -lqscintilla2_qt5 # Installed system-wide so no -L is needed
CONFIG += qscintilla2
