include ../common/Makefile.def

TARGET = state-server$(EXE)

SOURCES = \
    src/main.cpp 

INCLUDE += -Iinclude

LIB += -lpthread -lboost_system
CXX_FLAGS += #-DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
