include ../common/Makefile.def

TARGET = dc2800-codec-test$(EXE)

SOURCES = \
    src/AnalogInputsLoadTest.cpp \
    src/AnalogInputsSaveTest.cpp \
    src/CommandEncodeTest.cpp \
    ../battery/src/Codec.cpp \
    ../battery/src/Command.cpp \
    src/main.cpp

INCLUDE= -I$(HOME)/binary/boost/include -I../battery/include -Iinclude
LIB= -L$(HOME)/binary/boost/lib

CXX_FLAGS += -std=c++11 

include ../common/Makefile.in
