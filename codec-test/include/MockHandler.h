/*
 * MockHandler.h
 *
 *  Created on: May 3, 2016
 *      Author: master
 */

#ifndef INCLUDE_MOCKHANDLER_H_
#define INCLUDE_MOCKHANDLER_H_

#include "Codec.h"

namespace codec {

class MockHandler : public boost::enable_shared_from_this<MockHandler>,
private boost::noncopyable {
public:
	typedef boost::shared_ptr<MockHandler> Ptr;

	MockHandler();
	virtual ~MockHandler();

	void handle(Context&,	boost::any&);
	void sessionStart(Context&);
	void sessionClose(Context&);
	void exceptionCaught(Context&, const std::exception&);

	void checkHandleCalled(boost::function<void (bool)> check);
	void checkSessionStartCalled(boost::function<void (bool)> check);
	void checkSessionCloseCalled(boost::function<void (bool)> check);
	void checkExceptionCaughtCalled(boost::function<void (bool)> check);

	HandlerPtr getHandler();
private:
	bool handleCalled;
	bool sessionStartCalled;
	bool sessionCloseCalled;
	bool exceptionCaughtCalled;
};

}

#endif /* INCLUDE_MOCKHANDLER_H_ */
