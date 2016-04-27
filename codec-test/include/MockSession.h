/*
 * MockSession.h
 *
 *  Created on: Apr 27, 2016
 *      Author: master
 */

#ifndef INCLUDE_MOCKSESSION_H_
#define INCLUDE_MOCKSESSION_H_

#include "Codec.h"

class MockSession : public boost::enable_shared_from_this<MockSession>,
private boost::noncopyable {
public:
	MockSession();
	virtual ~MockSession();

	codec::SessionPtr getSession();
	void close();

	void run();
private:
	void read(codec::Session::ReadBuffer,
			codec::Session::IoCompHandler);
	void write(codec::Session::WriteBuffer,
			codec::Session::IoCompHandler);
	void post(codec::Session::Task);

	boost::asio::io_service io_service;
	boost::asio::streambuf buffer;
};

#endif /* INCLUDE_MOCKSESSION_H_ */
