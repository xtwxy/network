/*
 * MyMessageHandler.h
 *
 *  Created on: May 3, 2016
 *      Author: master
 */

#ifndef INCLUDE_MYMESSAGEHANDLER_H_
#define INCLUDE_MYMESSAGEHANDLER_H_

#include "Codec.h"

class MyMessageHandler : public boost::enable_shared_from_this<MyMessageHandler>,
private boost::noncopyable {
public:
	typedef boost::shared_ptr<MyMessageHandler> Ptr;

	MyMessageHandler();
	virtual ~MyMessageHandler();

	void handle(codec::Context&, boost::any&);
	void sessionStart(codec::Context&);
	void sessionClose(codec::Context&);
	void exceptionCaught(codec::Context&, const std::exception&);

	codec::HandlerPtr getHandler();
};

#endif /* INCLUDE_MYMESSAGEHANDLER_H_ */
