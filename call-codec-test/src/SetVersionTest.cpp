#include <boost/test/unit_test.hpp>
#include <boost/make_shared.hpp>
#include <iostream>
#include <iomanip>
#include <ctime>

#include "CallProtocol.h"
#include "MessageCodec.h"
#include "MessageHandler.h"
#include "MyMessage.h"
#include "MyMessageHandler.h"

#include "Utils.h"

using namespace std;
using namespace boost;
using namespace CallProtocol;

BOOST_AUTO_TEST_SUITE( SetVersionTest )

const static ProtocolVersion PROTOCOL_VERSION = 0x00000001;
MessageHandlerFactory::Ptr messageFactory;
MessageHandlerFactoryVersions factoryVersions;

struct SetVersionTestConfig {
    SetVersionTestConfig()   {
    	std::cout << "global setup\n";
    	messageFactory = boost::make_shared<MessageHandlerFactory>();
    	MessageHandler::Ptr rootHandler = boost::make_shared<MessageHandler>(factoryVersions);
    	SetVersionRequestHandler::Ptr requestHandler = boost::make_shared<SetVersionRequestHandler>(rootHandler);
    	SetVersionResponseHandler::Ptr responseHandler = boost::make_shared<SetVersionResponseHandler>(rootHandler);

    	messageFactory->addHandler(SetVersionRequest::TYPE_ID, requestHandler->getHandler());
    	messageFactory->addHandler(SetVersionResponse::TYPE_ID, responseHandler->getHandler());
    	factoryVersions.insert(make_pair(PROTOCOL_VERSION, messageFactory));
    }

    ~SetVersionTestConfig()  { std::cout << "global teardown\n"; }
};

BOOST_GLOBAL_FIXTURE( SetVersionTestConfig );

BOOST_AUTO_TEST_CASE( testSetVersion ) {
	MessageType TYPE_ID = SetVersionRequest::TYPE_ID;

	Message<SetVersionRequest> message;
	message.setLength(sizeof(message));
	message.setTypeId(TYPE_ID);

	codec::HandlerPtr handler = messageFactory->getHandler(SetVersionRequest::TYPE_ID);
	boost::asio::io_service ios;
	codec::Pipeline pipeline(ios);
	codec::Context ctx(pipeline);

	boost::any out(message);
	handler->handle(ctx, out);

}

BOOST_AUTO_TEST_SUITE_END()


