/*
 * call_proto.h
 *
 *  Created on: May 24, 2016
 *      Author: master
 */

#ifndef INCLUDE_CALLPROTOCOL_H_
#define INCLUDE_CALLPROTOCOL_H_

#include <map>
#include <boost/endian/buffers.hpp>
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>

namespace CallProtocol {

typedef uint16_t MessageType;
typedef uint16_t Correlation;

struct MessageHeader {
	MessageHeader() {}
	uint16_t getLength();
	void setLength(uint16_t);

	MessageType getType();
	void setType(const MessageType);

	boost::endian::little_uint16_buf_t length;
	boost::endian::little_uint16_buf_t type;
};

struct Message {
	uint16_t getLength();
	void setLength(uint16_t);

	MessageType getType();
	void setType(const MessageType);

	MessageHeader header;
};

struct OnewayMessage : public Message {
};

struct TwowayMessage : public Message {

	Correlation getCorrelation();
	void setCorrelation(Correlation c);

	boost::endian::little_uint16_buf_t correlation;
};

typedef boost::shared_ptr<Message> MessagePtr;
typedef boost::function<MessagePtr (MessageType)> MessageCreator;

class MessageFactory {
public:
	MessageFactory();
	~MessageFactory();

	MessagePtr createMessage(const MessageType);
private:
	std::map<MessageType, MessageCreator> msgCreators;
};


} // namespace CallProtocol

#endif /* INCLUDE_CALLPROTOCOL_H_ */
