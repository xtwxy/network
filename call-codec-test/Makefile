include ../common/Makefile.def

TARGET = call-codec-test$(EXE)

SOURCES = \
    ../call-codec/src/CallProtocol.cpp \
    ../codec/src/Codec.cpp \
    src/MyMessage.cpp \
    src/MyMessageHandler.cpp \
    src/CallProtocolTest.cpp \
    src/main.cpp 

INCLUDE += -Iinclude -I../call-codec/include -I../codec/include

LIB += -lpthread 

CXX_FLAGS += -std=c++11 #-DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
