include ../common/Makefile.def

TARGET = battery$(EXE)

SOURCES = \
    src/main.cpp 

INCLUDE += -Iinclude 

LIB += -lpthread -L/usr/local/library -lrestbed 

CXX_FLAGS += -std=c++11 #-DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
