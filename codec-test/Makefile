include ../common/Makefile.def

TARGET = codec-test$(EXE)

SOURCES = \
    src/TestDataElement.cpp \
    src/InitializerTest.cpp \
    src/ValidatorTest.cpp \
    src/ObserverTest.cpp \
    src/main.cpp

INCLUDE += -Iinclude -I../codec/include

LIB += -lpthread -lboost_unit_test_framework
CXX_FLAGS += #-DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
