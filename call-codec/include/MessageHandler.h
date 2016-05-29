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

class MessageHandler : public boost::enable_shared_from_this<MessageHandler>,
private boost::noncopyable {
public:
	typedef boost::shared_ptr<MessageHandler> Ptr;
	MessageHandler(MessageHandlerFactory::Ptr);
	virtual ~MessageHandler();

	void handle(codec::Context&, boost::any&);
	void sessionStart(codec::Context&);
	void sessionClose(codec::Context&);
	void exceptionCaught(codec::Context&, const std::exception&);

codec::HandlerPtr getHandler();
private:
	MessageHandlerFactory::Ptr msgHandlerFactory;
};

} /* namespace CallProtocol */

#endif /* INCLUDE_MESSAGEHANDLER_H_ */
