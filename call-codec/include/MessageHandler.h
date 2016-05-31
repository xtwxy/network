/*
 * MessageHandler.h
 *
 *  Created on: May 26, 2016
 *      Author: master
 */

#ifndef INCLUDE_MESSAGEHANDLER_H_
#define INCLUDE_MESSAGEHANDLER_H_

#include "Codec.h"
#include "CallProtocol.h"

namespace CallProtocol {

struct SetVersionRequest {
  typedef boost::shared_ptr<SetVersionRequest> Ptr;
	const static CallProtocol::MessageType TYPE_ID = 0;
	boost::endian::little_uint32_buf_t version;
	uint32_t getVersion();
	void setVersion(uint32_t);
};

struct SetVersionResponse {
  typedef boost::shared_ptr<SetVersionResponse> Ptr;
	const static CallProtocol::MessageType TYPE_ID = 1;
	enum StatusCode { OK = 0, NOT_SUPPORTED_VERSION = 1 };
	boost::endian::little_uint32_buf_t statusCode;
	uint32_t getStatusCode();
	void setStatusCode(uint32_t);
};
class MessageHandler;
typedef boost::shared_ptr<MessageHandler> MessageHandlerPtr;
typedef std::map<ProtocolVersion, MessageHandlerFactory::Ptr> MessageHandlerFactoryVersions;

class SetVersionRequestHandler : public boost::enable_shared_from_this<SetVersionRequestHandler>,
private boost::noncopyable {
public:
	typedef boost::shared_ptr<SetVersionRequestHandler> Ptr;
	SetVersionRequestHandler(MessageHandlerPtr);
	virtual ~SetVersionRequestHandler();

	void handle(codec::Context&, boost::any&);
	void sessionStart(codec::Context&);
	void sessionClose(codec::Context&);
	void exceptionCaught(codec::Context&, const std::exception&);

	codec::HandlerPtr getHandler();
private:
	MessageHandlerPtr rootMessageHandler;
};


class SetVersionResponseHandler : public boost::enable_shared_from_this<SetVersionResponseHandler>,
private boost::noncopyable {
public:
	typedef boost::shared_ptr<SetVersionResponseHandler> Ptr;
	SetVersionResponseHandler(MessageHandlerPtr);
	virtual ~SetVersionResponseHandler();

	void handle(codec::Context&, boost::any&);
	void sessionStart(codec::Context&);
	void sessionClose(codec::Context&);
	void exceptionCaught(codec::Context&, const std::exception&);

	codec::HandlerPtr getHandler();
private:
	MessageHandlerPtr rootMessageHandler;
};

class MessageHandler : public boost::enable_shared_from_this<MessageHandler>,
private boost::noncopyable {
public:
	typedef boost::shared_ptr<MessageHandler> Ptr;
	MessageHandler(const MessageHandlerFactoryVersions& version);
	virtual ~MessageHandler();

	void handle(codec::Context&, boost::any&);
	void sessionStart(codec::Context&);
	void sessionClose(codec::Context&);
	void exceptionCaught(codec::Context&, const std::exception&);

	codec::HandlerPtr getHandler();
	void setVersion(ProtocolVersion);
private:
	MessageHandlerFactory::Ptr msgHandlerFactory;
	const MessageHandlerFactoryVersions& msgHandlerFactoryVersions;
};

} /* namespace CallProtocol */

#endif /* INCLUDE_MESSAGEHANDLER_H_ */
