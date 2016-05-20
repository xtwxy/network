
#include <algorithm>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

#include "echo_service.hpp"

#include <RCF/RCF.hpp>

int main(int argc, char* argv[])
{
    RCF::RcfInitDeinit rcfInit;

    if(argc != 2) {
      std::cout << "Usage: " << argv[0] << " <HOST/IP>" << std::endl;
    }
    try
    {
        std::string s = "Hello, World!";

        // Make the call.
        RcfClient<echo_service> service( RCF::TcpEndpoint(argv[1], 50001) );
        while(true) service.echo(s);
    }
    catch(const RCF::Exception & e)
    {
        std::cout << "Caught exception:\n";
        std::cout << e.getError().getErrorString() << std::endl;
        return 1;
    }

    return 0;
}
