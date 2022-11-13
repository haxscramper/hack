QT       += core gui widgets

TARGET = OgreQT5
TEMPLATE = app

INCLUDEPATH *= /usr/include/OGRE

SOURCES += *.cpp
HEADERS  += *.hpp

DISTFILES *= plugins.cfg

#DEFINES *= OGRE_NODELESS_POSITIONING

LIBS += -lboost_system
LIBS += -lOgreMain -lOgreBites -lOgreTerrain

