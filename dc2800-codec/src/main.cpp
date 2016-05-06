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
#include "FrameCodec.h"
#include "DataCodec.h"
#include "DataHandler.h"

using namespace codec;
using namespace nw;
using namespace dc2800_codec;

void DC2800PipelineInitializer(codec::Pipeline& pipeline) {
	FrameCodec::Ptr frameCodec = boost::make_shared<FrameCodec>();
	DataCodec::Ptr dataCodec = boost::make_shared<DataCodec>();
	DataHandler::Ptr dataHandler = boost::make_shared<DataHandler>();

	pipeline.addLast(frameCodec->getCodec());
	pipeline.addLast(dataCodec->getCodec());
	pipeline.setHandler(dataHandler->getHandler());
}

int main(int argc, char* argv[]) {
	boost::asio::io_service ios;

	nw::Acceptor::Ptr acceptor(new nw::Acceptor(ios));
	PipelineInitializer initializer = &DC2800PipelineInitializer;
	acceptor->setPort(2001).setPipelineInitializer(initializer);
	acceptor->start();
	ios.run();

	return EXIT_SUCCESS;
}

