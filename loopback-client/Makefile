include ../common/Makefile.def

TARGET = loopback-client$(EXE)

SOURCES = \
    src/main.cpp 

INCLUDE += -Iinclude

LIB += -lpthread
CXX_FLAGS +=-DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
