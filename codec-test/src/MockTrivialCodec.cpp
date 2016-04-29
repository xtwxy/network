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

CodecPtr MockTrivialCodec::getCodec() {
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

void MockTrivialCodec::encode(Context& ctx, boost::any& input, std::list<boost::any>& output) {
	output.push_back(input);
	encodeCalled = true;
}

void MockTrivialCodec::decode(Context& ctx, boost::any& input, std::list<boost::any>& output) {
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

void MockTrivialCodec::checkPostCondition() {
	BOOST_CHECK_EQUAL(encodeCalled, true);
	BOOST_CHECK_EQUAL(decodeCalled, true);
	BOOST_CHECK_EQUAL(sessionStartCalled, true);
	BOOST_CHECK_EQUAL(sessionCloseCalled, true);
	BOOST_CHECK_EQUAL(exceptionCaughtCalled, true);
}

} /* namespace codec */
