/*
 * MessageHandler.cpp
 *
 *  Created on: May 26, 2016
 *      Author: master
 */
#include <boost/bind.hpp>
#include <boost/make_shared.hpp>

#include "MessageHandler.h"

namespace CallProtocol {

uint32_t SetVersionRequest::getVersion() {
	return version.value();
}
void SetVersionRequest::setVersion(uint32_t v) {
	version = v;
}

uint32_t SetVersionResponse::getStatusCode() {
	return statusCode.value();
}
void SetVersionResponse::setStatusCode(uint32_t v) {
	statusCode = v;
}

SetVersionRequestHandler::SetVersionRequestHandler(MessageHandlerPtr h)
:rootHandler(h) {
}

SetVersionRequestHandler::~SetVersionRequestHandler() {

}

void SetVersionRequestHandler::handle(codec::Context& ctx, boost::any& msg) {
	if (msg.type() == typeid(MessagePtr)) {
		MessagePtr msgPtr = boost::any_cast<MessagePtr>(msg);
		assert(msgPtr->getTypeId() == SetVersionRequest::TYPE_ID);
		assert(msgPtr->getLength() == sizeof(SetVersionRequest) + sizeof(MessageHeader));

		SetVersionRequestMessagePtr reqMsgPtr = boost::reinterpret_pointer_cast<SetVersionRequestMessage>(msgPtr);
		SetVersionResponseMessagePtr respMsgPtr = boost::make_shared<SetVersionResponseMessage>();
		boost::any out = respMsgPtr;
		ctx.write(out);
	} else {
		// Cannot be possible!
		assert(false);
	}
}

void SetVersionRequestHandler::sessionStart(codec::Context&) {

}

void SetVersionRequestHandler::sessionClose(codec::Context&) {

}

void SetVersionRequestHandler::exceptionCaught(codec::Context&, const std::exception&) {

}

codec::HandlerPtr SetVersionRequestHandler::getHandler() {
	codec::HandlerFunc handler = boost::bind(&SetVersionRequestHandler::handle,
			shared_from_this(), _1, _2);
	codec::SessionStart sessionStart = boost::bind(
			&SetVersionRequestHandler::sessionStart, shared_from_this(), _1);
	codec::SessionClose sessionClose = boost::bind(
			&SetVersionRequestHandler::sessionClose, shared_from_this(), _1);
	codec::ExceptionCaught exceptionCaught = boost::bind(
			&SetVersionRequestHandler::exceptionCaught, shared_from_this(), _1, _2);
	codec::HandlerPtr ptr(
			new codec::Handler(handler, sessionStart, sessionClose,
					exceptionCaught));
	return ptr;
}

SetVersionResponseHandler::SetVersionResponseHandler(MessageHandlerPtr h)
: rootMessageHandler(h) {
}

SetVersionResponseHandler::~SetVersionResponseHandler() {

}

void SetVersionResponseHandler::handle(codec::Context& ctx, boost::any& msg) {
	if (msg.type() == typeid(MessagePtr)) {
		MessagePtr msgPtr = boost::any_cast<MessagePtr>(msg);
		assert(msgPtr->getTypeId() == SetVersionRequest::TYPE_ID);
		assert(msgPtr->getLength() == sizeof(SetVersionResponse) + sizeof(MessageHeader));

		SetVersionResponseMessagePtr respMsgPtr = boost::reinterpret_pointer_cast<SetVersionResponseMessage>(msgPtr);
		rootMessageHandler->setVersion(respMsgPtr->payload.getStatusCode());
	} else {
		// Cannot be possible!
		assert(false);
	}
}

void SetVersionResponseHandler::sessionStart(codec::Context&) {

}

void SetVersionResponseHandler::sessionClose(codec::Context&) {

}

void SetVersionResponseHandler::exceptionCaught(codec::Context&, const std::exception&) {

}

codec::HandlerPtr SetVersionResponseHandler::getHandler() {
	codec::HandlerFunc handler = boost::bind(&SetVersionResponseHandler::handle,
			shared_from_this(), _1, _2);
	codec::SessionStart sessionStart = boost::bind(
			&SetVersionResponseHandler::sessionStart, shared_from_this(), _1);
	codec::SessionClose sessionClose = boost::bind(
			&SetVersionResponseHandler::sessionClose, shared_from_this(), _1);
	codec::ExceptionCaught exceptionCaught = boost::bind(
			&SetVersionResponseHandler::exceptionCaught, shared_from_this(), _1, _2);
	codec::HandlerPtr ptr(
			new codec::Handler(handler, sessionStart, sessionClose,
					exceptionCaught));
	return ptr;
}

MessageHandler::MessageHandler(const MessageHandlerFactoryVersions& version) :
		msgHandlerFactory(), msgHandlerFactoryVersions(version) {
	MessageHandlerFactory::Ptr messageFactoryInitial = boost::make_shared<MessageHandlerFactory>();

    	SetVersionRequestHandler::Ptr requestHandler = boost::make_shared<SetVersionRequestHandler>(shared_from_this());
    	SetVersionResponseHandler::Ptr responseHandler = boost::make_shared<SetVersionResponseHandler>(shared_from_this());

    	messageFactoryInitial->addHandler(SetVersionRequest::TYPE_ID, requestHandler->getHandler());
    	messageFactoryInitial->addHandler(SetVersionResponse::TYPE_ID, responseHandler->getHandler());
}

MessageHandler::~MessageHandler() {

}

void MessageHandler::handle(codec::Context& ctx, boost::any& msg) {
	if (msg.type() == typeid(MessagePtr)) {
		MessagePtr msgPtr = boost::any_cast<MessagePtr>(msg);
		codec::HandlerPtr handlerPtr = msgHandlerFactory->getHandler(
				msgPtr->getTypeId());
		handlerPtr->handle(ctx, msg);
	} else {
		// Cannot be possible!
		assert(false);
	}
}

void MessageHandler::sessionStart(codec::Context& ctx) {
	msgHandlerFactory.reset(new MessageHandlerFactory());
	SetVersionRequestHandler::Ptr requestHandler = boost::make_shared<SetVersionRequestHandler>(shared_from_this());
	SetVersionResponseHandler::Ptr responseHandler = boost::make_shared<SetVersionResponseHandler>(shared_from_this());

	msgHandlerFactory->addHandler(SetVersionRequest::TYPE_ID, requestHandler->getHandler());
	msgHandlerFactory->addHandler(SetVersionResponse::TYPE_ID, responseHandler->getHandler());
}

void MessageHandler::sessionClose(codec::Context& ctx) {

}

void MessageHandler::exceptionCaught(codec::Context& ctx,
		const std::exception& ex) {
	ctx.close();
}

codec::HandlerPtr MessageHandler::getHandler() {
	codec::HandlerFunc handler = boost::bind(&MessageHandler::handle,
			shared_from_this(), _1, _2);
	codec::SessionStart sessionStart = boost::bind(
			&MessageHandler::sessionStart, shared_from_this(), _1);
	codec::SessionClose sessionClose = boost::bind(
			&MessageHandler::sessionClose, shared_from_this(), _1);
	codec::ExceptionCaught exceptionCaught = boost::bind(
			&MessageHandler::exceptionCaught, shared_from_this(), _1, _2);
	codec::HandlerPtr ptr(
			new codec::Handler(handler, sessionStart, sessionClose,
					exceptionCaught));
	return ptr;
}

void MessageHandler::setVersion(ProtocolVersion v) {
	auto it = msgHandlerFactoryVersions.find(v);
	if(it != msgHandlerFactoryVersions.end()) {
		msgHandlerFactory = it->second;
	}
}

} /* namespace CallProtocol */
