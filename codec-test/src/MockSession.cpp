/*
 * MockSession.cpp
 *
 *  Created on: Apr 27, 2016
 *      Author: master
 */

#include <algorithm>
#include <boost/bind.hpp>
#include "MockSession.h"

using namespace codec;
using namespace boost;
using namespace boost::asio;

MockSession::MockSession() {
}

MockSession::~MockSession() {
}

SessionPtr MockSession::getSession() {
	SessionPtr ptr(
			new Session(
					bind(&MockSession::read, shared_from_this(), _1, _2),
					bind(&MockSession::write, shared_from_this(), _1, _2),
					bind(&MockSession::post, shared_from_this(), _1),
					bind(&MockSession::close, shared_from_this())
					)
			);
	return ptr;
}
void MockSession::read(Session::ReadBuffer buffer,
		Session::IoCompHandler handler) {
    char buff[256];
	io_service.post([&buff, &buffer, &handler, this]() {
		  std::size_t destSize = boost::asio::detail::buffer_size_helper(buffer);

		  Session::WriteBuffer src = this->buffer.data();
		  std::size_t srcSize = boost::asio::detail::buffer_size_helper(src);

		  std::size_t len = (destSize > srcSize) ? srcSize : destSize;

		  std::copy((char *)src.begin(), (char *)src.begin() + len, buff);

		  handler(boost::system::error_code(
				  	boost::system::errc::success,
					boost::system::get_system_category()),
				  len);
	});
}
void MockSession::write(Session::WriteBuffer buffer,
		Session::IoCompHandler handler) {

}
void MockSession::post(Session::Task t) {
	io_service.post(t);
}
void MockSession::close() {

}
void MockSession::run() {
	io_service.run();
}
