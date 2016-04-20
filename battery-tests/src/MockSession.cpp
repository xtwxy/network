/*
 * MockSession.cpp
 *
 *  Created on: Apr 20, 2016
 *      Author: master
 */

#include "MockSession.h"

MockSession::MockSession(char* rdbuf, std::size_t availableToRead,
		char* wrbuf, std::size_t wrbufSize)
: readBuffer_(rdbuf),
  availableToRead_(availableToRead),
  writeBuffer_(wrbuf),
  writeBufferSize_(wrbufSize),
  bytesRead_(0),
  bytesWritten_(0) {
}

MockSession::~MockSession() {
}

void MockSession::read(char* buff, std::size_t len, battery::Session::Handler handler) {
	std::size_t toRead = len;
	std::size_t available = (availableToRead_ - bytesRead_);
	if(len > available) {
		toRead = available;
	}
	for(std::size_t i = 0; i != toRead; ++i) {
		buff[i] = readBuffer_[bytesRead_ + i];
	}
	bytesRead_ += toRead;
}

void MockSession::write(char* buff, std::size_t len, battery::Session::Handler handler) {
	std::size_t toRead = len;
	std::size_t available = (availableToRead_ - bytesRead_);
	if(len > available) {
		toRead = available;
	}
	for(std::size_t i = 0; i != toRead; ++i) {
		buff[i] = readBuffer_[bytesRead_ + i];
	}
	bytesRead_ += toRead;
}

void MockSession::readComplete(const boost::system::error_code& ec, std::size_t len) {

}

void MockSession::writeComplete(const boost::system::error_code& ec, std::size_t len) {

}
