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
#include "MessageCodec.h"
#include "MessageHandler.h"

using namespace codec;
using namespace nw;
using namespace CallProtocol;

void StateServerPipelineInitializer(codec::Pipeline& pipeline) {
	MessageCodec::Ptr messageCodec = boost::make_shared<MessageCodec>();
	MessageHandler::Ptr messageHandler = boost::make_shared<MessageHandler>();

	pipeline.addLast(messageCodec->getCodec());
	pipeline.setHandler(messageHandler->getHandler());
}

int main(int argc, char* argv[]) {
	boost::asio::io_service ios;

	nw::Acceptor::Ptr acceptor(new nw::Acceptor(ios));
	PipelineInitializer initializer = &StateServerPipelineInitializer;
	acceptor->setPort(2001).setPipelineInitializer(initializer);
	acceptor->start();
	ios.run();

	return EXIT_SUCCESS;
}

