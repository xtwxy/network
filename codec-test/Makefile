include ../common/Makefile.def

TARGET = codec-test$(EXE)

SOURCES = \
     src/MockSession.cpp \
     src/PipelineTest.cpp \
     src/SessionTest.cpp \
    ../codec/src/Codec.cpp \
    src/main.cpp

#   src/TestDataElement.cpp \
#   src/InitializerTest.cpp \
#    src/ValidatorTest.cpp \
#    src/ObserverTest.cpp \


INCLUDE += -Iinclude -I../codec/include

LIB += -lpthread -lboost_unit_test_framework
CXX_FLAGS += -std=c++11 #-DPRINT_BUFFER #-DBOOST_ASIO_ENABLE_HANDLER_TRACKING

include ../common/Makefile.in
