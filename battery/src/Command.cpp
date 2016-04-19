/*
 * Command.cpp
 *
 *  Created on: Apr 19, 2016
 *      Author: master
 */

#include "Command.h"

namespace battery {

Command::Command(nw::Connection::Ptr conn,
		Encoder encoder,
		Decoder decoder,
		CompletionHandler handler)
: conn_(conn),
  encoder_(encoder),
  decoder_(decoder),
  handler_(handler),
  BUFFER_SIZE(2048){
	readBuffer_ = new char[BUFFER_SIZE];
	writeBuffer_ = new char[BUFFER_SIZE];
}

Command::~Command() {
	delete[] readBuffer_;
	delete[] writeBuffer_;
}

void Command::execute() {
	encodeHeader();
}

} /* namespace battery */
