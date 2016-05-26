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

class Message;
class MessageFactory;

typedef boost::shared_ptr<Message> MessagePtr;
typedef boost::shared_ptr<MessageFactory> MessageFactoryPtr;

struct MessageHeader {
	MessageHeader() {}
	uint16_t getLength() const;
	void setLength(uint16_t);

	MessageType getTypeId() const;
	void setTypeId(const MessageType);

	boost::endian::little_uint16_buf_t length;
	boost::endian::little_uint16_buf_t typeId;
};

struct Message {
	uint16_t getLength() const;
	void setLength(uint16_t);

	MessageType getTypeId() const;
	void setTypeId(const MessageType);

	MessageHeader header;
};

template<typename T>
struct OnewayMessage : public Message {
	typedef boost::shared_ptr<OnewayMessage> Ptr;
	OnewayMessage() {
		setLength(sizeof(*this));
		setTypeId(T::TYPE_ID);
	}
	bool isOneway() const { return true; };

	T payload;
};

template<typename T>
struct TwowayMessage : public Message {
	typedef boost::shared_ptr<TwowayMessage> Ptr;
	TwowayMessage() : correlation() {
		setLength(sizeof(*this));
		setTypeId(T::TYPE_ID);
	}
	Correlation getCorrelation() const { return correlation.value(); }
	void setCorrelation(Correlation c) { correlation = c; }
	bool isOneway() const { return false; }

	boost::endian::little_uint16_buf_t correlation;
	T payload;
};

class MessageFactory {
public:

	MessageFactory();
	virtual ~MessageFactory();

	virtual MessagePtr createMessage() const = 0;
};

class CodecMessageFactory {
public:
	CodecMessageFactory();
	virtual ~CodecMessageFactory();

	MessagePtr createMessage(const MessageType) const;
	void add(const MessageType, MessageFactoryPtr);
private:
	std::map<MessageType, MessageFactoryPtr> delegates;
};

} // namespace CallProtocol

#endif /* INCLUDE_CALLPROTOCOL_H_ */
