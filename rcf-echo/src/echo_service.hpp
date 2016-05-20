
#ifndef INCLUDE_MYSERVICE_HPP
#define INCLUDE_MYSERVICE_HPP

#include <string>
#include <vector>

#include <RCF/Idl.hpp>
#include <SF/vector.hpp>

RCF_BEGIN(echo_service, "echo_service")
    RCF_METHOD_R1(std::string, echo, std::string &);
RCF_END(echo_service);

#endif // ! INCLUDE_MYSERVICE_HPP
