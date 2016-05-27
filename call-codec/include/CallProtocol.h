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

#include <Codec.h>

namespace CallProtocol {

typedef uint16_t MessageType;
typedef uint16_t Correlation;

class Message;
class MessageHandlerFactory;
typedef boost::shared_ptr<Message> MessagePtr;
typedef boost::shared_ptr<MessageHandlerFactory> MessageHandlerFactoryPtr;

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

class MessageHandlerFactory {
public:
	typedef boost::shared_ptr<MessageHandlerFactory> Ptr;
	MessageHandlerFactory();
	virtual ~MessageHandlerFactory();

	void addHandler(const MessageType, codec::HandlerPtr);
	codec::HandlerPtr getHandler(const MessageType) const;
private:
	std::map<MessageType, codec::HandlerPtr> messageHandlers;
};

} // namespace CallProtocol

#endif /* INCLUDE_CALLPROTOCOL_H_ */
