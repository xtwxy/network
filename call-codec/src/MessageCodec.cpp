/*
 * MessageCodec.cpp
 *
 *  Created on: May 26, 2016
 *      Author: master
 */
#include <iostream>
#include <boost/bind.hpp>
#include <boost/make_shared.hpp>
#include <string.h>
#include "MessageCodec.h"

namespace CallProtocol {

MessageCodec::MessageCodec()
:state(DECODE_HEADER) {

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
	CharSequencePtr msg = boost::any_cast<CharSequencePtr>(input);
	codec::BufferPtr psb = boost::shared_ptr<boost::asio::streambuf>();

	const char* byteRepr = reinterpret_cast<const char *>(msg.get());
	std::size_t len = msg->getLength();
	psb->sputn(byteRepr, len);

	output.push_back(psb);
}

void MessageCodec::decode(codec::Context& ctx, boost::any& input, std::list<boost::any>& output) {
	bool hasData = true;
	do {
		if(state == DECODE_HEADER) {
			decodeHeader(ctx, input, output);
		} else if(state == DECODE_BODY){
			decodeBody(ctx, input, output);
		}
	} while(hasData);
}

void MessageCodec::sessionStart(codec::Context& ctx) {

}

void MessageCodec::sessionClose(codec::Context& ctx) {

}

void MessageCodec::exceptionCaught(codec::Context& ctx, const std::exception& ex) {
	ctx.close();
}

bool MessageCodec::decodeHeader(codec::Context& ctx, boost::any& input, std::list<boost::any>& output) {
	  codec::BufferPtr psb = boost::any_cast<codec::BufferPtr>(input);
	  if(psb->size() < sizeof(MessageHeader)) {
		  return false;
	  } else {
		  psb->sgetn(reinterpret_cast<char*>(&header), sizeof(header));
		  state = DECODE_BODY;
	  }
	  return true;
}

bool MessageCodec::decodeBody(codec::Context& ctx, boost::any& input, std::list<boost::any>& output) {
	  codec::BufferPtr psb = boost::any_cast<codec::BufferPtr>(input);
	  const std::size_t length = (header.getLength() - sizeof(MessageHeader));
	  if(psb->size() < length) {
		  return false;
	  } else {
		  char *cp = new char[header.getLength()];
		  memcpy(cp, &header, sizeof(MessageHeader));
		  psb->sgetn(cp + sizeof(MessageHeader), length);
		  CharSequencePtr msg;
		  msg.reset(reinterpret_cast<MessageHeader*>(cp));
		  output.push_back(msg);
		  state = DECODE_HEADER;
	  }
	  return true;
}


} /* namespace CallProtocol */
