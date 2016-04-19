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
  Header* hdr = reinterpret_cast<Header*>(writeBuffer_);
  hdr->Len = sizeof(Header);

  char *data = (writeBuffer_ + sizeof(Header));
  if(encoder_(data, 
           BUFFER_SIZE, 
           hdr->Addr, 
           hdr->PackNo, 
           hdr->Cmd, 
           hdr->Len)) {
    // encode success, add tail section (checksum, EOI),
    Tail* tail = reinterpret_cast<Tail*>(writeBuffer_ + hdr->Len.value());
    tail->Chksum = checksum(writeBuffer_, hdr->Len.value());
    // and deliver package!
    
  } else {
    // encode failed, notify the requester.
    handler_(boost::system::errc::make_error_code(boost::system::errc::bad_message));
  }
}

uint16_t Command::checksum(char* buff, std::size_t len) {
  return 0;
}
} /* namespace battery */
