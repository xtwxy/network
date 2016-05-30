/*
 * MessageCodec.cpp
 *
 *  Created on: May 26, 2016
 *      Author: master
 */
#include <iostream>
#include <boost/bind.hpp>
#include <boost/make_shared.hpp>

#include "MessageCodec.h"

namespace CallProtocol {

MessageCodec::MessageCodec()
:strategy(boost::bind(&MessageCodec::decodeHeader, shared_from_this(), _1, _2, _3)) {

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
	MessagePtr msg = boost::any_cast<MessagePtr>(input);
	codec::BufferPtr psb = boost::shared_ptr<boost::asio::streambuf>();

	const char* byteRepr = reinterpret_cast<const char *>(msg.get());
	std::size_t len = msg->getLength();
	psb->sputn(byteRepr, len);

	output.push_back(psb);
}

void MessageCodec::decode(codec::Context& ctx, boost::any& input, std::list<boost::any>& output) {
	while(strategy(ctx, input, output));
}

void MessageCodec::sessionStart(codec::Context& ctx) {

}

void MessageCodec::sessionClose(codec::Context& ctx) {

}

void MessageCodec::exceptionCaught(codec::Context& ctx, const std::exception& ex) {
	ctx.close();
}

bool MessageCodec::decodeHeader(codec::Context& ctx, boost::any& input, std::list<boost::any>& output) {
	  codec::BufferPtr psb = boost::shared_ptr<boost::asio::streambuf>();
	  if(psb->size() < sizeof(MessageHeader)) {
		  return false;
	  } else {
		  psb->sgetn(reinterpret_cast<char*>(&header), sizeof(header));
		  strategy = boost::bind(&MessageCodec::decodeBody, shared_from_this(),  _1, _2, _3);
	  }
	  return true;
}

bool MessageCodec::decodeBody(codec::Context& ctx, boost::any& input, std::list<boost::any>& output) {
	  codec::BufferPtr psb = boost::shared_ptr<boost::asio::streambuf>();
	  const std::size_t length = (header.getLength() - sizeof(MessageHeader));
	  if(psb->size() < length) {
		  return false;
	  } else {
		  char *cp = new char[length];
		  psb->sgetn(cp, length);
		  MessagePtr msg;
		  msg.reset(reinterpret_cast<MessageHeader*>(cp));
		  output.push_back(msg);
		  strategy = boost::bind(&MessageCodec::decodeHeader, shared_from_this(),  _1, _2, _3);
	  }
	  return true;
}


} /* namespace CallProtocol */
