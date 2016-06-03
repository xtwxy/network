include ../common/Makefile.def

TARGET = call-codec$(EXE)

SOURCES = \
    src/MessageHandler.cpp \
    src/MessageCodec.cpp \
    src/CallProtocol.cpp \
    ../codec/src/Codec.cpp \
    src/main.cpp 

INCLUDE += -Iinclude -I../acceptor/include -I../codec/include

LIB += -lpthread -L/usr/local/library 

CXX_FLAGS += -std=c++11 #-DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
