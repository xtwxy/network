#include <boost/test/unit_test.hpp>
#include <boost/make_shared.hpp>
#include <iostream>
#include <iomanip>
#include <ctime>

#include "CallProtocol.h"
#include "MyMessage.h"
#include "MyMessageHandler.h"

using namespace std;
using namespace boost;
using namespace CallProtocol;

#ifdef NDEBUG
#define PRINT_MESSAGE(message)
#else
#define PRINT_MESSAGE(message) {                                                             \
const unsigned char* p = reinterpret_cast<const unsigned char *>(&message);                  \
cout << #message << ": " << typeid(message).name() << endl;                                                                    \
for(size_t i = 0; i != sizeof(message); ++i) {                                               \
	cout << "0x"<< hex << setw(2) << setfill('0') << static_cast<unsigned short>(*(p + i)) << ", ";  \
}                                                                                            \
cout << endl;                                                                                \
}
#endif

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
struct MyConfig {
    MyConfig()   {
    	std::cout << "global setup\n";
    	MyMessageHandler::Ptr handler = boost::make_shared<MyMessageHandler>();
    	messageFactory.addHandler(MyMessage::TYPE_ID, handler->getHandler());
    }
    ~MyConfig()  { std::cout << "global teardown\n"; }
};

BOOST_GLOBAL_FIXTURE( MyConfig );

BOOST_AUTO_TEST_CASE( testMessageHeaderCodec ) {

	MessageHeader message;
	message.setLength(LENGTH);
	message.setTypeId(TYPE);

	BOOST_CHECK_EQUAL(message.getLength(), LENGTH);
	BOOST_CHECK_EQUAL(message.getTypeId(), TYPE);

	unsigned char repr[] = { 0xbe, 0xba, 0xfe, 0xca };
	BOOST_CHECK_EQUAL(memcmp(repr, &message, sizeof(repr)), 0);

	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_CASE( testMessageCodec ) {

	Message message;
	message.setLength(LENGTH);
	message.setTypeId(TYPE);

	BOOST_CHECK_EQUAL(message.getLength(), LENGTH);
	BOOST_CHECK_EQUAL(message.getTypeId(), TYPE);

	unsigned char repr[] = { 0xbe, 0xba, 0xfe, 0xca };
	BOOST_CHECK_EQUAL(memcmp(repr, &message, sizeof(repr)), 0);

	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_CASE( testOnewayMessageCodec ) {

	OnewayMessage<MyMessage> message;
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
			0xbe, 0xba, 0xfe, 0xca, 0x01,
			0x00, 0x00, 0x00, 0x94, 0x99,
			0x46, 0x57, 0x01, 0x40, 0x9f,
			0x30, 0x00, 0x00, 0x00, 0x00,
			0x00
	};
	BOOST_CHECK_EQUAL(memcmp(repr, &message, sizeof(repr)), 0);

	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_CASE( testTwowayMessageCodec ) {
	const Correlation CORRELATION_ID = 0x7474;

	TwowayMessage<MyMessage> message;
	message.setCorrelation(CORRELATION_ID);
	message.setLength(LENGTH);
	message.setTypeId(TYPE);

	BOOST_CHECK_EQUAL(message.getLength(), LENGTH);
	BOOST_CHECK_EQUAL(message.getTypeId(), TYPE);

	BOOST_CHECK_EQUAL(message.getCorrelation(), CORRELATION_ID);


	message.payload.setAlarmId(ALARM_ID);
	message.payload.setTimestamp(TIMESTAMP);
	message.payload.setStatus(STATUS);
	message.payload.setCurrentValue(CURRENT_VALUE);

	BOOST_CHECK_EQUAL(message.payload.getAlarmId(), ALARM_ID);
	BOOST_CHECK_EQUAL(message.payload.getTimestamp(), TIMESTAMP);
	BOOST_CHECK_EQUAL(message.payload.getStatus(), STATUS);
	BOOST_CHECK_EQUAL(message.payload.getCurrentValue(), CURRENT_VALUE);

	unsigned char repr[] = {
			0xbe, 0xba, 0xfe, 0xca, 0x74,
			0x74, 0x01, 0x00, 0x00, 0x00,
			0x94, 0x99, 0x46, 0x57, 0x01,
			0x40, 0x9f, 0x30, 0x00, 0x00,
			0x00, 0x00, 0x00
	};
	BOOST_CHECK_EQUAL(memcmp(repr, &message, sizeof(repr)), 0);

	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_CASE( testMyOnewayMessageCodec ) {

	MyMessage message;

	message.setAlarmId(ALARM_ID);
	message.setTimestamp(TIMESTAMP);
	message.setStatus(STATUS);
	message.setCurrentValue(CURRENT_VALUE);

	BOOST_CHECK_EQUAL(message.getAlarmId(), ALARM_ID);
	BOOST_CHECK_EQUAL(message.getTimestamp(), TIMESTAMP);
	BOOST_CHECK_EQUAL(message.getStatus(), STATUS);
	BOOST_CHECK_EQUAL(message.getCurrentValue(), CURRENT_VALUE);

	unsigned char repr[] = {
			0x01, 0x00, 0x00, 0x00, 0x94,
			0x99, 0x46, 0x57, 0x01, 0x40,
			0x9f, 0x30, 0x00, 0x00, 0x00,
			0x00, 0x00
	};
	BOOST_CHECK_EQUAL(memcmp(repr, &message, sizeof(repr)), 0);

	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_CASE( testMyOnewayMessageFactory ) {

	OnewayMessage<MyMessage> message;
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


