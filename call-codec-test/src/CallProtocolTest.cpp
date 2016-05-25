#include <boost/test/unit_test.hpp>
#include <boost/make_shared.hpp>
#include <iostream>
#include <iomanip>
#include <ctime>

#include "CallProtocol.h"

using namespace std;
using namespace boost;
using namespace CallProtocol;

#define PRINT_MESSAGE(message) {                                                             \
const unsigned char* p = reinterpret_cast<const unsigned char *>(&message);                  \
cout << #message << ": " << typeid(message).name() << endl;                                                                    \
for(size_t i = 0; i != sizeof(message); ++i) {                                               \
	cout << hex << setw(2) << setfill('0') << static_cast<unsigned short>(*(p + i)) << " ";  \
}                                                                                            \
cout << endl;                                                                                \
}

const size_t LENGTH = 0xbabe;
const MessageType TYPE = 0xcafe;

BOOST_AUTO_TEST_SUITE( CallProtocolTest )

struct MyOnewayMessage : public OnewayMessage {
	boost::endian::little_uint32_buf_t alarmId;
	boost::endian::little_uint32_buf_t timestamp;
	boost::endian::little_int8_buf_t status;
	boost::endian::big_uint64_buf_t currentValue;

	uint32_t getAlarmId() const {
		return alarmId.value();
	}

	void setAlarmId(uint32_t alarmId) {
		this->alarmId = alarmId;
	}

	double getCurrentValue() const {
		uint64_t v = currentValue.value();
		return *reinterpret_cast<double*>(&v);
	}

	void setCurrentValue(double cv) {
		uint64_t v = *reinterpret_cast<unsigned long int*>(&cv);
		this->currentValue = v;
	}

	uint8_t getStatus() const {
		return status.value();
	}

	void setStatus(uint8_t status) {
		this->status = status;
	}

	uint32_t getTimestamp() const {
		return timestamp.value();
	}

	void setTimestamp(uint32_t timestamp) {
		this->timestamp = timestamp;
	}
};

BOOST_AUTO_TEST_CASE( testMessageHeaderCodec ) {

	MessageHeader message;
	message.setLength(LENGTH);
	message.setType(TYPE);

	BOOST_CHECK_EQUAL(message.getLength(), LENGTH);
	BOOST_CHECK_EQUAL(message.getType(), TYPE);


	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_CASE( testMessageCodec ) {

	Message message;
	message.setLength(LENGTH);
	message.setType(TYPE);

	BOOST_CHECK_EQUAL(message.getLength(), LENGTH);
	BOOST_CHECK_EQUAL(message.getType(), TYPE);

	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_CASE( testOnewayMessageCodec ) {

	OnewayMessage message;
	message.setLength(LENGTH);
	message.setType(TYPE);

	BOOST_CHECK_EQUAL(message.getLength(), LENGTH);
	BOOST_CHECK_EQUAL(message.getType(), TYPE);

	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_CASE( testTwowayMessageCodec ) {
	const Correlation CORRELATION_ID = 0x7474;

	TwowayMessage message;
	message.setCorrelation(CORRELATION_ID);
	message.setLength(LENGTH);
	message.setType(TYPE);

	BOOST_CHECK_EQUAL(message.getLength(), LENGTH);
	BOOST_CHECK_EQUAL(message.getType(), TYPE);

	BOOST_CHECK_EQUAL(message.getCorrelation(), CORRELATION_ID);

	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_CASE( testMyOnewayMessageCodec ) {

	const uint_least32_t ALARM_ID = 1;
	const uint_least32_t TIMESTAMP = time(nullptr);
	const uint_least8_t STATUS = 1;
	const double CURRENT_VALUE = 1996;

	MyOnewayMessage message;
	message.setLength(LENGTH);
	message.setType(TYPE);

	BOOST_CHECK_EQUAL(message.getLength(), LENGTH);
	BOOST_CHECK_EQUAL(message.getType(), TYPE);
	message.setAlarmId(ALARM_ID);
	message.setTimestamp(TIMESTAMP);
	message.setStatus(STATUS);
	message.setCurrentValue(CURRENT_VALUE);

	BOOST_CHECK_EQUAL(message.getAlarmId(), ALARM_ID);
	BOOST_CHECK_EQUAL(message.getTimestamp(), TIMESTAMP);
	BOOST_CHECK_EQUAL(message.getStatus(), STATUS);
	BOOST_CHECK_EQUAL(message.getCurrentValue(), CURRENT_VALUE);

	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_SUITE_END()


