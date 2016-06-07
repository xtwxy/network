include ../common/Makefile.def

TARGET = state-server$(EXE)

SOURCES = \
    ../codec/src/Codec.cpp \
    ../call-codec/src/CallProtocol.cpp \
    ../call-codec/src/MessageCodec.cpp \
    ../call-codec/src/MessageHandler.cpp \ 
    src/main.cpp

INCLUDE += -Iinclude -I../codec/include -I../call-codec/include

LIB += -lpthread -lboost_system
CXX_FLAGS += -std=c++11 #-DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
