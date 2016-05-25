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
	bool isOneway() const { return true; };

	T payload;
};

template<typename T>
struct TwowayMessage : public Message {
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

	virtual Message* createMessage(const MessageType) = 0;
	virtual void deleteMessage(const Message*) = 0;
	virtual void setMessageTypeId(const MessageType) = 0;
};

class CodecMessageFactory : public MessageFactory {
public:
	Message* createMessage(const MessageType);
	void deleteMessage(const Message*);
	void setMessageTypeId(const MessageType);

	void add(MessageFactory*);
	static CodecMessageFactory& getInstance();
private:
	CodecMessageFactory();
	virtual ~CodecMessageFactory();
	std::map<MessageType, MessageFactory*> delegates;
	static MessageType typeCount;
	static CodecMessageFactory instance;
};

} // namespace CallProtocol

#endif /* INCLUDE_CALLPROTOCOL_H_ */
