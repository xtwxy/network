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

struct CallProtocolTestConfig {
    CallProtocolTestConfig()   {
    	std::cout << "global setup\n";
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
	Correlation correlation = 1;
	MessageType TYPE_ID = MyMessage::TYPE_ID;
	MyMessage::Ptr myMessage = boost::make_shared<MyMessage>();

	myMessage->setAlarmId(ALARM_ID);
	myMessage->setTimestamp(TIMESTAMP);
	myMessage->setStatus(STATUS);
	myMessage->setCurrentValue(CURRENT_VALUE);

	BOOST_CHECK_EQUAL(myMessage->getAlarmId(), ALARM_ID);
	BOOST_CHECK_EQUAL(myMessage->getTimestamp(), TIMESTAMP);
	BOOST_CHECK_EQUAL(myMessage->getStatus(), STATUS);
	BOOST_CHECK_EQUAL(myMessage->getCurrentValue(), CURRENT_VALUE);

	unsigned char repr[] = {
			0xbe, 0xba, 0xfe, 0xca, 0x00,
			0x00, 0x01, 0x00, 0x00, 0x00,
			0x94, 0x99, 0x46, 0x57, 0x01,
			0x40, 0x9f, 0x30, 0x00, 0x00,
			0x00, 0x00, 0x00
	};

	PayloadPtr payload = myMessage;
	Message message(payload->size(), MyMessage::TYPE_ID, correlation, payload);

	BOOST_CHECK_EQUAL(message.getLength(), payload->size());
	BOOST_CHECK_EQUAL(message.getTypeId(), TYPE_ID);

	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_SUITE_END()


