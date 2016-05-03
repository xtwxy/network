/*
 * MockHandler.cpp
 *
 *  Created on: May 3, 2016
 *      Author: master
 */

#include <boost/bind.hpp>
#include "MockHandler.h"

namespace codec {

MockHandler::MockHandler():
	handleCalled(false),
	sessionStartCalled(false),
	sessionCloseCalled(false),
	exceptionCaughtCalled(false) {

}

MockHandler::~MockHandler() {
}

void MockHandler::handle(Context&,	boost::any&){
	handleCalled = true;
}

void MockHandler::sessionStart(Context&){
	sessionStartCalled = true;
}

void MockHandler::sessionClose(Context&){
	sessionCloseCalled = true;
}

void MockHandler::exceptionCaught(Context&, const std::exception&){
	exceptionCaughtCalled = true;
}

void MockHandler::checkHandleCalled(boost::function<void (bool)> check) {
	check(handleCalled);
}

void MockHandler::checkSessionStartCalled(boost::function<void (bool)> check) {
	check(sessionStartCalled);
}

void MockHandler::checkSessionCloseCalled(boost::function<void (bool)> check) {
	check(sessionCloseCalled);
}

void MockHandler::checkExceptionCaughtCalled(boost::function<void (bool)> check) {
	check(exceptionCaughtCalled);
}

HandlerPtr MockHandler::getHandler() {
	HandlerFunc handler = boost::bind(&MockHandler::handle, shared_from_this(), _1, _2);
	SessionStart sessionStart = boost::bind(&MockHandler::sessionStart, shared_from_this(), _1);
	SessionClose sessionClose = boost::bind(&MockHandler::sessionClose, shared_from_this(), _1);
	ExceptionCaught exceptionCaught = boost::bind(&MockHandler::exceptionCaught, shared_from_this(), _1, _2);
	HandlerPtr ptr(new Handler(
				handler,
				sessionStart,
				sessionClose,
				exceptionCaught
			));
	return ptr;
}

}

