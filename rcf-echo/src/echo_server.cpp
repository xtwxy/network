
#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

#include <RCF/RCF.hpp>

#include "echo_service.hpp"

class echo_serviceImpl
{
public:
 std::string echo(std::string& s) {
   return s;
 }
};

int main()
{
    RCF::RcfInitDeinit rcfInit;

    // Start a TCP server on port 50001, and expose echo_serviceImpl.
    echo_serviceImpl myServiceImpl;
    RCF::RcfServer server( RCF::TcpEndpoint("0.0.0.0", 50001) );
    RCF::ThreadPoolPtr threadPoolPtr( new RCF::ThreadPool(1, 8) );
    server.setThreadPool(threadPoolPtr);
    server.bind<echo_service>(myServiceImpl);
    server.start();

    std::cout << "Press Enter to exit..." << std::endl;
    std::cin.get();

    return 0;
}
