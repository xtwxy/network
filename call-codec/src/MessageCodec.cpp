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

MessageCodec::MessageCodec(PayloadFactoryPtr factory)
  :message(boost::make_shared<Message>()),
   action(boost::bind(&MessageCodec::decodeHeader, shared_from_this(), _1, _2, _3)),
   payloadFactory(factory) {

}

MessageCodec::~MessageCodec() {

}

codec::CodecPtr MessageCodec::createCodec(PayloadFactoryPtr factory) {
	Ptr ptr = boost::make_shared<MessageCodec>(factory);
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

	psb->sputn(reinterpret_cast<const char*>(&msg->header), sizeof(msg->header));
	msg->payload->store(*psb);

	output.push_back(psb);
}

void MessageCodec::decode(codec::Context& ctx, boost::any& input, std::list<boost::any>& output) {
	while(action(ctx, input, output));
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
		  psb->sgetn(reinterpret_cast<char*>(&message->header), sizeof(MessageHeader));
		 
      action = boost::bind(&MessageCodec::decodeBody, shared_from_this(), _1, _2, _3);
	  }
	  return true;
}

bool MessageCodec::decodeBody(codec::Context& ctx, boost::any& input, std::list<boost::any>& output) {
	  codec::BufferPtr psb = boost::any_cast<codec::BufferPtr>(input);
	  const std::size_t length = (message->header.getLength() - sizeof(MessageHeader));
	  if(psb->size() < length) {
		  return false;
	  } else {
		  message->payload = payloadFactory->createPayload(message->header.getTypeId());
		  message->payload->load(*psb);
		  output.push_back(message);
		  
		  action = boost::bind(&MessageCodec::decodeHeader, shared_from_this(), _1, _2, _3);
		  message = boost::make_shared<Message>();
	  }
	  return true;
}


} /* namespace CallProtocol */
