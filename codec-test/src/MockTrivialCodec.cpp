/*
 * MockTrivialCodec.cpp
 *
 *  Created on: Apr 29, 2016
 *      Author: master
 */
#include <boost/bind.hpp>
#include <boost/make_shared.hpp>
#include <boost/test/unit_test.hpp>
#include "MockTrivialCodec.h"

namespace codec {

MockTrivialCodec::MockTrivialCodec() :
		encodeCalled(false),
		decodeCalled(false),
		sessionStartCalled(false),
		sessionCloseCalled(false),
		exceptionCaughtCalled(false) {

}

MockTrivialCodec::~MockTrivialCodec() {

}

CodecPtr MockTrivialCodec::createCodec() {
	Ptr codec = boost::make_shared<MockTrivialCodec>();
	EncodeFunc encoder = boost::bind(&MockTrivialCodec::encode, codec,
			_1, _2,	_3);
	EncodeFunc decoder = boost::bind(&MockTrivialCodec::decode, codec,
			_1, _2,	_3);

	SessionStart ssnStart = boost::bind(&MockTrivialCodec::sessionStart, codec,
			_1);
	SessionClose ssnClose = boost::bind(&MockTrivialCodec::sessionClose, codec,
			_1);

	ExceptionCaught exceptCaught = boost::bind(
			&MockTrivialCodec::exceptionCaught, codec, _1, _2);

	CodecPtr ptr(new Codec(encoder, decoder, ssnStart, ssnClose, exceptCaught));

	return ptr;
}


CodecPtr MockTrivialCodec::getCodec() {
	EncodeFunc encoder = boost::bind(&MockTrivialCodec::encode, shared_from_this(),
			_1, _2,	_3);
	EncodeFunc decoder = boost::bind(&MockTrivialCodec::decode, shared_from_this(),
			_1, _2,	_3);

	SessionStart ssnStart = boost::bind(&MockTrivialCodec::sessionStart, shared_from_this(),
			_1);
	SessionClose ssnClose = boost::bind(&MockTrivialCodec::sessionClose, shared_from_this(),
			_1);

	ExceptionCaught exceptCaught = boost::bind(
			&MockTrivialCodec::exceptionCaught, shared_from_this(), _1, _2);

	CodecPtr ptr(new Codec(encoder, decoder, ssnStart, ssnClose, exceptCaught));

	return ptr;
}

void MockTrivialCodec::encode(Context& ctx, boost::any& input, std::list<boost::any>& output) {
	  boost::asio::streambuf* psb = boost::any_cast<boost::asio::streambuf*>(input);
	  std::size_t len = psb->size();

	  std::cout << this << " encode(): psb->size() = " << len << std::endl;

	  output.push_back(input);
	encodeCalled = true;
}

void MockTrivialCodec::decode(Context& ctx, boost::any& input, std::list<boost::any>& output) {
	  boost::asio::streambuf* psb = boost::any_cast<boost::asio::streambuf*>(input);
	  std::size_t len = psb->size();

	  std::cout << this << " decode(): psb->size() = " << len << std::endl;

	output.push_back(input);
	decodeCalled = true;
}

void MockTrivialCodec::sessionStart(Context& ctx) {
	sessionStartCalled = true;
}

void MockTrivialCodec::sessionClose(Context& ctx) {
	sessionCloseCalled = true;
}

void MockTrivialCodec::exceptionCaught(Context& ctx, const std::exception& ex) {
	exceptionCaughtCalled = true;
}

void MockTrivialCodec::checkEncodeCalled(boost::function<void (bool)> check) {
	check(encodeCalled);
}

void MockTrivialCodec::checkDecodeCalled(boost::function<void (bool)> check) {
	check(decodeCalled);
}

void MockTrivialCodec::checkSessionStartCalled(boost::function<void (bool)> check) {
	check(sessionStartCalled);
}

void MockTrivialCodec::checkSessionCloseCalled(boost::function<void (bool)> check) {
	check(sessionCloseCalled);
}

void MockTrivialCodec::checkExceptionCaughtCalled(boost::function<void (bool)> check) {
	check(exceptionCaughtCalled);
}

} /* namespace codec */
