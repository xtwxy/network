include ../common/Makefile.def

TARGET = battery-tests$(EXE)

SOURCES = \
    src/AnalogInputsLoadTest.cpp \
    src/AnalogInputsSaveTest.cpp \
    src/main.cpp

INCLUDE= -I$(HOME)/binary/boost/include -I../battery/include
LIB= -L$(HOME)/binary/boost/lib

CXX_FLAGS += -std=c++11 

include ../common/Makefile.in
