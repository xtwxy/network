/*
 * MessageCodec.cpp
 *
 *  Created on: May 26, 2016
 *      Author: master
 */
#include <boost/bind.hpp>
#include <boost/make_shared.hpp>

#include "MessageCodec.h"

namespace CallProtocol {

MessageCodec::MessageCodec() {

}

MessageCodec::~MessageCodec() {

}

codec::CodecPtr MessageCodec::createCodec() {
	Ptr ptr = boost::make_shared<MessageCodec>();
	return ptr->getCodec();
}

codec::CodecPtr MessageCodec::getCodec() {
	codec::EncodeFunc encoder = boost::bind(&MessageCodec::encode, shared_from_this(),	_1, _2,	_3);
	codec::EncodeFunc decoder = boost::bind(&MessageCodec::decode, shared_from_this(), _1, _2,	_3);

	codec::SessionStart ssnStart = boost::bind(&MessageCodec::sessionStart, shared_from_this(), _1);
	codec::SessionClose ssnClose = boost::bind(&MessageCodec::sessionClose, shared_from_this(), _1);

	codec::ExceptionCaught exceptCaught = boost::bind(&MessageCodec::exceptionCaught, shared_from_this(), _1, _2);

	codec::CodecPtr ptr(new codec::Codec(encoder, decoder, ssnStart, ssnClose, exceptCaught));

	return ptr;
}

void MessageCodec::encode(codec::Context& ctx, boost::any& input, std::list<boost::any>& output) {

}

void MessageCodec::decode(codec::Context& ctx, boost::any& input, std::list<boost::any>& ouput) {

}

void MessageCodec::sessionStart(codec::Context& ctx) {

}

void MessageCodec::sessionClose(codec::Context& ctx) {

}

void MessageCodec::exceptionCaught(codec::Context& ctx, const std::exception& ex) {
	ctx.close();
}


} /* namespace CallProtocol */
