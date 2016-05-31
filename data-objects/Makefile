include ../common/Makefile.def

TARGET = data-objects$(EXE)

SOURCES = \
    ../codec/src/Codec.cpp \
    ../call-codec/src/CallProtocol.cpp \
    src/main.cpp 

INCLUDE += -Iinclude -I../codec/include -I../call-codec/include

LIB += -lpthread -lboost_system
CXX_FLAGS += #-DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
