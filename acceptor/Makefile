include ../common/Makefile.def

TARGET = acceptor$(EXE)

SOURCES = \
   ../codec/src/Codec.cpp \
   ../codec-test/src/MockHandler.cpp \
   src/main.cpp 

INCLUDE += -Iinclude -I../codec/include -I../codec-test/include

LIB += -lpthread
CXX_FLAGS +=-std=c++11 -DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
