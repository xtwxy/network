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
#include <boost/shared_array.hpp>
#include <boost/make_shared.hpp>

#include <Codec.h>

namespace CallProtocol {

typedef uint16_t MessageType;
typedef uint16_t Correlation;
typedef uint32_t ProtocolVersion;

typedef boost::shared_array<char> CharSequencePtr;

struct MessageHeader;
class Payload;
class PayloadFactory;
struct Message;
class MessageHandlerFactory;

typedef boost::shared_ptr<MessageHeader> MessageHeaderPtr;
typedef boost::shared_ptr<Payload> PayloadPtr;
typedef boost::shared_ptr<PayloadFactory> PayloadFactoryPtr;
typedef boost::shared_ptr<Message> MessagePtr;

typedef boost::function<PayloadPtr (const MessageType)> PayloadCreator;

struct MessageHeader {
	MessageHeader();
	MessageHeader(uint32_t len,
			MessageType type,
			Correlation c);
	uint32_t getLength() const;
	void setLength(uint32_t);

	MessageType getTypeId() const;
	void setTypeId(const MessageType);

	Correlation getCorrelation() const;
	void setCorrelation(Correlation c);

	boost::endian::little_uint32_buf_t length;
	boost::endian::little_uint16_buf_t typeId;
	boost::endian::little_uint16_buf_t correlation;
};

class Payload : public boost::enable_shared_from_this<Payload> {
public:
	typedef boost::shared_ptr<Payload> Ptr;
	Payload ();
	virtual ~Payload();

	boost::any any();
	virtual void load(boost::asio::streambuf&) = 0;
	virtual void store(boost::asio::streambuf&) = 0;
	virtual std::size_t size() = 0;
};

struct Message {
	Message();
	Message(uint32_t len,
			MessageType type,
			Correlation c,
			Payload::Ptr payload);
	uint32_t getLength() const;
	void setLength(uint32_t l);

	MessageType getTypeId() const;
	void setTypeId(const MessageType t);

	Correlation getCorrelation() const;
	void setCorrelation(Correlation c);

	MessageHeader header;
	Payload::Ptr payload;
};

class PayloadFactory {
public:
	PayloadFactory();
	virtual ~PayloadFactory();

	void addCreator(const MessageType, const PayloadCreator);
	PayloadPtr createPayload(const MessageType) const;
private:
	std::map<MessageType, PayloadCreator> payloadCreators;
};

} // namespace CallProtocol

#endif /* INCLUDE_CALLPROTOCOL_H_ */
