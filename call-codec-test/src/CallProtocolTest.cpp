#include <boost/test/unit_test.hpp>
#include <boost/make_shared.hpp>
#include <iostream>
#include <iomanip>
#include <ctime>

#include "CallProtocol.h"
#include "MyMessage.h"
#include "MyMessageHandler.h"
#include "Utils.h"

using namespace std;
using namespace boost;
using namespace CallProtocol;

// for all message headers.
const size_t LENGTH = 0xbabe;
const MessageType TYPE = 0xcafe;

// for all MyMessage instances.
const uint_least32_t ALARM_ID = 1;
const uint_least32_t TIMESTAMP = 0x57469994;// time(nullptr);
const uint_least8_t STATUS = 1;
const double CURRENT_VALUE = 1996;

BOOST_AUTO_TEST_SUITE( CallProtocolTest )

MessageHandlerFactory messageFactory;
struct CallProtocolTestConfig {
    CallProtocolTestConfig()   {
    	std::cout << "global setup\n";
    	MyMessageHandler::Ptr handler = boost::make_shared<MyMessageHandler>();
    	messageFactory.addHandler(MyMessage::TYPE_ID, handler->getHandler());
    }
    ~CallProtocolTestConfig()  { std::cout << "global teardown\n"; }
};

BOOST_GLOBAL_FIXTURE( CallProtocolTestConfig );

BOOST_AUTO_TEST_CASE( testMessageHeaderCodec ) {

	MessageHeader message;
	message.setLength(LENGTH);
	message.setTypeId(TYPE);

	BOOST_CHECK_EQUAL(message.getLength(), LENGTH);
	BOOST_CHECK_EQUAL(message.getTypeId(), TYPE);

	unsigned char repr[] = { 0xbe, 0xba, 0xfe, 0xca, 0x00, 0x00 };
	BOOST_CHECK_EQUAL(memcmp(repr, &message, sizeof(message)), 0);

	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_CASE( testOnewayMessageCodec ) {

	Message<MyMessage> message;
	message.setLength(LENGTH);
	message.setTypeId(TYPE);

	BOOST_CHECK_EQUAL(message.getLength(), LENGTH);
	BOOST_CHECK_EQUAL(message.getTypeId(), TYPE);

	message.payload.setAlarmId(ALARM_ID);
	message.payload.setTimestamp(TIMESTAMP);
	message.payload.setStatus(STATUS);
	message.payload.setCurrentValue(CURRENT_VALUE);

	BOOST_CHECK_EQUAL(message.payload.getAlarmId(), ALARM_ID);
	BOOST_CHECK_EQUAL(message.payload.getTimestamp(), TIMESTAMP);
	BOOST_CHECK_EQUAL(message.payload.getStatus(), STATUS);
	BOOST_CHECK_EQUAL(message.payload.getCurrentValue(), CURRENT_VALUE);

	unsigned char repr[] = {
			0xbe, 0xba, 0xfe, 0xca, 0x00,
			0x00, 0x01, 0x00, 0x00, 0x00,
			0x94, 0x99, 0x46, 0x57, 0x01,
			0x40, 0x9f, 0x30, 0x00, 0x00,
			0x00, 0x00, 0x00
	};
	BOOST_CHECK_EQUAL(memcmp(repr, &message, sizeof(message)), 0);

	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_CASE( testMyOnewayMessageFactory ) {

	Message<MyMessage> message;
	message.setLength(LENGTH);
	message.setTypeId(TYPE);

	message.payload.setAlarmId(ALARM_ID);
	message.payload.setTimestamp(TIMESTAMP);
	message.payload.setStatus(STATUS);
	message.payload.setCurrentValue(CURRENT_VALUE);


	MessageType TYPE_ID = MyMessage::TYPE_ID;
	codec::HandlerPtr handler = messageFactory.getHandler(MyMessage::TYPE_ID);
	boost::asio::io_service ios;
	codec::Pipeline pipeline(ios);
	codec::Context ctx(pipeline);

	boost::any out(message);
	handler->handle(ctx, out);

}

BOOST_AUTO_TEST_SUITE_END()


