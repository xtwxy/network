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

void SetVersionRequest::load(std::streambuf& sb) {
	sb.sgetn(reinterpret_cast<char*>(&version), sizeof(version));
}

void SetVersionRequest::store(std::streambuf& sb) {
	sb.sputn(version.data(), sizeof(version));
}

std::size_t SetVersionRequest::size() {
	return sizeof(version);
}

uint32_t SetVersionResponse::getStatusCode() {
	return statusCode.value();
}
void SetVersionResponse::setStatusCode(uint32_t v) {
	statusCode = v;
}

void SetVersionResponse::load(std::streambuf& sb) {
	sb.sgetn(reinterpret_cast<char*>(&statusCode), sizeof(statusCode));
}

void SetVersionResponse::store(std::streambuf& sb) {
	sb.sputn(statusCode.data(), sizeof(statusCode));
}

std::size_t SetVersionResponse::size() {
	return sizeof(statusCode);
}

MessageHandler::MessageHandler() {

}

MessageHandler::~MessageHandler() {

}

void MessageHandler::handle(codec::Context& ctx, boost::any& msg) {
	if (msg.type() == typeid(MessagePtr)) {
		MessagePtr msgPtr = boost::any_cast<MessagePtr>(msg);
	} else {
		// Cannot be possible!
		assert(false);
	}
}

void MessageHandler::sessionStart(codec::Context& ctx) {
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

} /* namespace CallProtocol */
