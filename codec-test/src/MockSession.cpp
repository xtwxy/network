/*
 * MockSession.cpp
 *
 *  Created on: Apr 27, 2016
 *      Author: master
 */

#include <iostream>
#include <algorithm>
#include <boost/bind.hpp>
#include "MockSession.h"

using namespace std;
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
					boost::bind(&MockSession::read, shared_from_this(), _1, _2),
					boost::bind(&MockSession::write, shared_from_this(), _1,
							_2),
					boost::bind(&MockSession::post, shared_from_this(), _1),
					boost::bind(&MockSession::close, shared_from_this())));
	return ptr;
}

void MockSession::read(Session::ReadBuffer rbuf,
		Session::IoCompHandler handler) {
	post([rbuf, handler, this](){
		boost::asio::streambuf::const_buffers_type cbufs = echoBuffer.data();

		const_buffer cb = *(cbufs.begin());
		std::size_t csize = boost::asio::detail::buffer_size_helper(cb);
		const char* cdata =
				reinterpret_cast<const char*>(boost::asio::detail::buffer_cast_helper(
						cb));

		mutable_buffer b = *(rbuf.begin());
		std::size_t size = boost::asio::detail::buffer_size_helper(b);
		char* data =
				reinterpret_cast<char*>(boost::asio::detail::buffer_cast_helper(b));

		std::size_t len = (size > csize) ? csize : size;
		std::copy(cdata, (cdata + len), data);

		echoBuffer.consume(len);
		handler(
				boost::system::error_code(boost::system::errc::success,
						boost::system::get_system_category()), len);
	});
}

void MockSession::write(Session::WriteBuffer wbuf,
		Session::IoCompHandler handler) {

	post([wbuf, handler, this](){
		boost::asio::streambuf::const_buffers_type::const_iterator cit =
				wbuf.begin();
		for (; cit != wbuf.end(); ++cit) {
			const_buffer cb = *(cit);
			std::size_t csize = boost::asio::detail::buffer_size_helper(cb);

			const char* cdata =
					reinterpret_cast<const char*>(boost::asio::detail::buffer_cast_helper(
							cb));

			boost::asio::streambuf::mutable_buffers_type wbufs = echoBuffer.prepare(
					csize);

			mutable_buffer wb = *(wbufs.begin());
			char* wdata =
					reinterpret_cast<char*>(boost::asio::detail::buffer_cast_helper(
							wb));

			std::copy(cdata, (cdata + csize), wdata);

			echoBuffer.commit(csize);

			handler(
					boost::system::error_code(boost::system::errc::success,
							boost::system::get_system_category()), csize);

			break;
		}
	});
}

void MockSession::post(Session::Task t) {
	io_service.post(t);
}

void MockSession::close() {

}

void MockSession::run() {
	io_service.run();
}
