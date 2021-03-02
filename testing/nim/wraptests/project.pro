SOURCES = $$files(*.cpp, true)
HEADERS = $$files(*.hpp, true)
DISTFILES = $$files(*.nim, true)

INCLUDEPATH *= /home/test/.choosenim/toolchains/nim-1.4.2/lib

INCLUDEPATH *=  /mnt/workspace/github/hax-nim/wraptests/callback_override

