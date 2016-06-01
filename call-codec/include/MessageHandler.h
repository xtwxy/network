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

class SetVersionRequest : public Payload {
public:
  typedef boost::shared_ptr<SetVersionRequest> Ptr;
	const static CallProtocol::MessageType TYPE_ID = 0;

	uint32_t getVersion();
	void setVersion(uint32_t);

	void load(std::streambuf&);
	void store(std::streambuf&);
	std::size_t size();
private:
	boost::endian::little_uint32_buf_t version;
};

class SetVersionResponse : public Payload {
public:
  typedef boost::shared_ptr<SetVersionResponse> Ptr;
	const static CallProtocol::MessageType TYPE_ID = 1;
	enum StatusCode { OK = 0, NOT_SUPPORTED_VERSION = 1 };

	uint32_t getStatusCode();
	void setStatusCode(uint32_t);

	void load(std::streambuf&);
	void store(std::streambuf&);
	std::size_t size();
private:
	boost::endian::little_uint32_buf_t statusCode;
};

class MessageHandler : public boost::enable_shared_from_this<MessageHandler>,
private boost::noncopyable {
public:
	typedef boost::shared_ptr<MessageHandler> Ptr;
	MessageHandler();
	virtual ~MessageHandler();

	void handle(codec::Context&, boost::any&);
	void sessionStart(codec::Context&);
	void sessionClose(codec::Context&);
	void exceptionCaught(codec::Context&, const std::exception&);

	codec::HandlerPtr getHandler();
private:
};

} /* namespace CallProtocol */

#endif /* INCLUDE_MESSAGEHANDLER_H_ */
