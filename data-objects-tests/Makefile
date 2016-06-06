include ../common/Makefile.def

TARGET = data-objects-tests$(EXE)

SOURCES = \
    ../codec/src/Codec.cpp \
    ../call-codec/src/CallProtocol.cpp \
    ../data-objects/src/SignalState.cpp \
    src/SignalStateTest.cpp \
    src/main.cpp 

INCLUDE += -Iinclude -I../codec/include -I../call-codec/include -I../data-objects/include

LIB += -lpthread -lboost_system
CXX_FLAGS +=-std=c++11 #-DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
