
ADD_DEFINITIONS( ${RCF_DEFINES} )

INCLUDE_DIRECTORIES( ${RCF_INCLUDES} )

ADD_EXECUTABLE(
    echo_server 
    ${SRC_PATH}/echo_server.cpp)

TARGET_LINK_LIBRARIES( echo_server RcfLib ${RCF_LIBS} )
