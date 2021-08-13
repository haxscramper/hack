SOURCES *= *.c *.cpp

QMAKE_CXXFLAGS_DEBUG *= -pg
QMAKE_LFLAGS_DEBUG *= -pg
CONFIG *= debug

DISTFILES += \
    Makefile

