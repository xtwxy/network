include ../common/Makefile.def

TARGET = strand-test$(EXE)

SOURCES = \
    src/main.cpp 

INCLUDE += -Iinclude

LIB += -lpthread -lboost_thread
CXX_FLAGS += -std=c++11 #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
