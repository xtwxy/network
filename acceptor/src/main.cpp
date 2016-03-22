#include <cstdlib>
#include <iostream>
#include <map>
#include <string>
#include <boost/asio.hpp>
#include <boost/asio/buffer.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

#include <boost/date_time/posix_time/posix_time.hpp>
#include "nw/Acceptor.hpp"

int main(int argc, char* argv[]) {
	boost::asio::io_service ios;
	
	nw::Acceptor::Ptr acceptor(new nw::Acceptor(
				ios,
				2001,
				4096,
				30
			));
	acceptor->start();
	ios.run();
	return EXIT_SUCCESS;
}

