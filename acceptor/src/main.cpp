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

	EchoHandler() { }
	virtual ~EchoHandler() { }

	void handle(Context& ctx,	boost::any& data)  {
		BufferPtr psb = boost::any_cast<BufferPtr>(data);
		std::size_t len = psb->size();
		if(len == 0) return;
		BufferPtr buff = boost::make_shared<boost::asio::streambuf>();
		boost::asio::streambuf::mutable_buffers_type mbs = buff->prepare(len);
		boost::asio::mutable_buffer mb = *(mbs.begin());
		std::size_t mbsize = boost::asio::detail::buffer_size_helper(mb);
		char* mbdata = reinterpret_cast<char*>(boost::asio::detail::buffer_cast_helper(mb));

		std::size_t index = 0;
		boost::asio::streambuf::const_buffers_type cbs = psb->data();
		for(auto it = cbs.begin(); it != cbs.end(); ++it) {
			boost::asio::const_buffer cb = *(it);
			std::size_t cbsize = boost::asio::detail::buffer_size_helper(cb);
			const char* cbdata = reinterpret_cast<const char*>(boost::asio::detail::buffer_cast_helper(cb));
			std::size_t icb = 0;
			for(; icb != cbsize; ++icb) {
				mbdata[index + icb] = cbdata[icb];
			}
#ifdef PRINT_BUFFER	
			print_buffer(cbdata, cbsize);
#endif // PRINT_BUFFER
			index += icb;
		}
		assert(index == len);
		buff->commit(len);
		psb->consume(len);

		boost::any out = buff;

		ctx.write(out);
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
};

int main(int argc, char* argv[]) {
	boost::asio::io_service ios;
	
	nw::Acceptor::Ptr acceptor(new nw::Acceptor(ios));

	acceptor->setPort(9080).setPipelineInitializer([](codec::Pipeline& pipeline){
		EchoHandler::Ptr echoHandler = boost::make_shared<EchoHandler>();
		pipeline.setHandler(echoHandler->getHandler());
	});
	acceptor->start();
	ios.run();
	return EXIT_SUCCESS;
}

