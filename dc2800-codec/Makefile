include ../common/Makefile.def

TARGET = dc2800-codec$(EXE)

SOURCES = \
    ../codec/src/Codec.cpp \
    src/FrameCodec.cpp \
    src/DataCodec.cpp \
    src/DataHandler.cpp \
    src/main.cpp 

INCLUDE += -Iinclude -I../acceptor/include -I../codec/include

LIB += -lpthread -L/usr/local/library -lrestbed 

CXX_FLAGS += -std=c++11 #-DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
