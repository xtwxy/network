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

struct MessageHeader;
class MessageHandlerFactory;
typedef boost::shared_ptr<MessageHeader> MessagePtr;
typedef boost::shared_ptr<MessageHandlerFactory> MessageHandlerFactoryPtr;

struct MessageHeader {
	MessageHeader() : length(), typeId(), correlation() { }
	uint16_t getLength() const;
	void setLength(uint16_t);

	MessageType getTypeId() const;
	void setTypeId(const MessageType);

	Correlation getCorrelation() const;
	void setCorrelation(Correlation c);

	boost::endian::little_uint16_buf_t length;
	boost::endian::little_uint16_buf_t typeId;
	boost::endian::little_uint16_buf_t correlation;
};

template<typename Payload>
struct Message {
	Message() : header(), payload() { }
	uint16_t getLength() const { return header.getLength(); }
	void setLength(uint16_t l) { header.setLength(l); }

	MessageType getTypeId() const { return header.getTypeId(); }
	void setTypeId(const MessageType t) { header.setTypeId(t); }

	Correlation getCorrelation() const { return header.getCorrelation(); }
	void setCorrelation(Correlation c) { header.setCorrelation(c); }

	MessageHeader header;
	Payload payload;
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
