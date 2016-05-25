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

// for all message headers.
const size_t LENGTH = 0xbabe;
const MessageType TYPE = 0xcafe;

// for all MyMessage instances.
const uint_least32_t ALARM_ID = 1;
const uint_least32_t TIMESTAMP = time(nullptr);
const uint_least8_t STATUS = 1;
const double CURRENT_VALUE = 1996;

BOOST_AUTO_TEST_SUITE( CallProtocolTest )

struct MyMessage {
	boost::endian::little_uint32_buf_t alarmId;
	boost::endian::little_uint32_buf_t timestamp;
	boost::endian::little_int8_buf_t status;
	boost::endian::big_uint64_buf_t currentValue;
	const static int TYPE_ID = 100;

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

class MyOnewayMessageFactory: public MessageFactory {
public:
	MyOnewayMessageFactory() : typeId() { }
	virtual ~MyOnewayMessageFactory() { }

	Message* createMessage(const MessageType t) {
		if(t == typeId) {
			return new OnewayMessage<MyMessage>();
		}
		return nullptr;
	}
	void deleteMessage(const Message* m) {
		if(m->getTypeId() == typeId) {
			const OnewayMessage<MyMessage>* om =
					reinterpret_cast<const OnewayMessage<MyMessage>*>(m);
			if(om != nullptr) {
				delete om;
			} else {
				assert(false);
			}
		} else {
			assert(false);
		}
	};
	void setMessageTypeId(const MessageType t) { typeId = t; }
private:
	MessageType typeId;
};

struct MyConfig {
    MyConfig()   {
    	std::cout << "global setup\n";
    	CodecMessageFactory::getInstance().add(new MyOnewayMessageFactory());
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


	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_CASE( testMessageCodec ) {

	Message message;
	message.setLength(LENGTH);
	message.setTypeId(TYPE);

	BOOST_CHECK_EQUAL(message.getLength(), LENGTH);
	BOOST_CHECK_EQUAL(message.getTypeId(), TYPE);

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

	PRINT_MESSAGE(message);
}

BOOST_AUTO_TEST_SUITE_END()


