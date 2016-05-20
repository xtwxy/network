include ../common/Makefile.def

TARGET = protobuf-test$(EXE)

SOURCES = \
    gen/Header.pb.cpp \
    src/main.cpp 

INCLUDE += -Igen

LIB += -lpthread -lprotobuf
CXX_FLAGS += #-DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
