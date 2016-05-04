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
#include <boost/make_shared.hpp>

#include <boost/date_time/posix_time/posix_time.hpp>
#include "nw/Acceptor.hpp"

using namespace codec;
using namespace nw;

class EchoHandler : public boost::enable_shared_from_this<EchoHandler>,
private boost::noncopyable {
public:
	typedef boost::shared_ptr<EchoHandler> Ptr;

	EchoHandler() :buff() { }
	virtual ~EchoHandler() { }

	void handle(Context& ctx,	boost::any& data)  {
		  boost::asio::streambuf* psb = boost::any_cast<boost::asio::streambuf*>(data);
		  std::size_t len = psb->size();

		  std::istream is(psb);
		  psb->consume(len);
	}

	void sessionStart(Context& ctx) { }
	void sessionClose(Context& ctx) { }
	void exceptionCaught(Context& ctx, const std::exception& ex)  { }

	HandlerPtr getHandler() {
		HandlerFunc handler = boost::bind(&EchoHandler::handle, shared_from_this(), _1, _2);
		SessionStart sessionStart = boost::bind(&EchoHandler::sessionStart, shared_from_this(), _1);
		SessionClose sessionClose = boost::bind(&EchoHandler::sessionClose, shared_from_this(), _1);
		ExceptionCaught exceptionCaught = boost::bind(&EchoHandler::exceptionCaught, shared_from_this(), _1, _2);
		HandlerPtr ptr(new Handler(
					handler,
					sessionStart,
					sessionClose,
					exceptionCaught
				));
		return ptr;
	}
private:
	boost::asio::streambuf buff;
};


int main(int argc, char* argv[]) {
	boost::asio::io_service ios;
	
	nw::Acceptor::Ptr acceptor(new nw::Acceptor(ios));
	EchoHandler::Ptr echoHandler = boost::make_shared<EchoHandler>();

	acceptor->setPort(2001).setPipelineInitializer([echoHandler](codec::Pipeline& pipeline){
		pipeline.setHandler(echoHandler->getHandler());
	});
	acceptor->start();
	ios.run();
	return EXIT_SUCCESS;
}

