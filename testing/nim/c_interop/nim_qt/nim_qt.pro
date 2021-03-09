QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

CONFIG += c++11
DEFINES += QT_DEPRECATED_WARNINGS

SOURCES += \
    main.cpp \
    mainwindow.cpp

HEADERS += \
    mainwindow.hpp

message(Project sources)
for(file, SOURCES):message($${file})

######################  generate c++ code from nim  #######################

DISTFILES += \
    lib.nim

system(nim cpp \
    --cc:gcc \
    --noMain \
    --noLinking \
    --header:lib.hpp \
    --nimcache:nimcache \
    lib.nim) {
    message("Generated c++ code from nim")
} else {
    error("Failed to generate c++ code")
}

#################  add generated sources to compilation  ##################

INCLUDEPATH *= $$PWD/nimcache/

HOME=$$system("echo $HOME")
NIM_VERSION=$$system("nim --version | grep Version | cut -d' ' -f4 | tr -d '\n'")

message(Home directory is $${HOME})
message(Detected nim version is $${NIM_VERSION})

INCLUDEPATH *= $$HOME/.choosenim/toolchains/nim-$$NIM_VERSION/lib

message(Includepath content)
for(dir, INCLUDEPATH):message(- $${dir})

SOURCES *= \
  $$PWD/nimcache/*.cpp

HEADERS *= \
  $$PWD/nimcache/*.h

message(All sources)
for(file, SOURCES):message(- $${file})
