/*
 * MyMessageHandler.cpp
 *
 *  Created on: May 3, 2016
 *      Author: master
 */

#include <boost/bind.hpp>
#include "MyMessageHandler.h"

using namespace codec;

MyMessageHandler::MyMessageHandler() {

}

MyMessageHandler::~MyMessageHandler() {
}

void MyMessageHandler::handle(Context& ctx,	boost::any& data){
}

void MyMessageHandler::sessionStart(Context&){
}

void MyMessageHandler::sessionClose(Context&){
}

void MyMessageHandler::exceptionCaught(Context&, const std::exception&){
}


HandlerPtr MyMessageHandler::getHandler() {
	HandlerFunc handler = boost::bind(&MyMessageHandler::handle, shared_from_this(), _1, _2);
	SessionStart sessionStart = boost::bind(&MyMessageHandler::sessionStart, shared_from_this(), _1);
	SessionClose sessionClose = boost::bind(&MyMessageHandler::sessionClose, shared_from_this(), _1);
	ExceptionCaught exceptionCaught = boost::bind(&MyMessageHandler::exceptionCaught, shared_from_this(), _1, _2);
	HandlerPtr ptr(new Handler(
				handler,
				sessionStart,
				sessionClose,
				exceptionCaught
			));
	return ptr;
}


