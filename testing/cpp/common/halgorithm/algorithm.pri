HEADERS *= \
    $$PWD/qwidget_cptr.hpp \
    $$PWD/qwidget_ptr.hpp \
    $$PWD/algorithm.hpp \
    $$PWD/algorithm_find.hpp \
    $$PWD/algorithm_pointers.hpp \
    $$PWD/algorithm_getters.hpp \
    $$PWD/algorithm_eraser.hpp \
    $$PWD/algorithm_for_each.hpp \
    $$PWD/algorithm_rangeoper.hpp \
    $$PWD/algorithm_fuzzy_string.hpp \
    $$PWD/setting_flags.hpp \
    $$PWD/include/algorithm/all.hpp \
    $$PWD/include/algorithm/qwidget_cptr.hpp \
    $$PWD/include/algorithm/qwidget_ptr.hpp \
    $$PWD/include/algorithm/setting_flags.hpp \
    $$PWD/include/algorithm/algorithm.hpp

SOURCES *= \
    $$PWD/algorithm_fuzzy_string.cpp

QMAKE_CXXFLAGS += -std=c++17

INCLUDEPATH *= $$PWD
INCLUDEPATH *= $$PWD/include
