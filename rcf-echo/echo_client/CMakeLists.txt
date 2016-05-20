
ADD_DEFINITIONS( ${RCF_DEFINES} )

INCLUDE_DIRECTORIES( ${RCF_INCLUDES} )

ADD_EXECUTABLE(
    echo_client
    ${SRC_PATH}/echo_client.cpp)

TARGET_LINK_LIBRARIES(echo_client RcfLib ${RCF_LIBS} )
