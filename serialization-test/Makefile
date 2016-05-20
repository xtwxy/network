include ../common/Makefile.def

TARGET = serialization-test$(EXE)

SOURCES = \
    src/main.cpp 

INCLUDE += -Iinclude

LIB += -lpthread -lboost_serialization
CXX_FLAGS += #-DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
