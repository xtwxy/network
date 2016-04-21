/*
 * Command.cpp
 *
 *  Created on: Apr 19, 2016
 *      Author: master
 */

#include <boost/bind.hpp>
#include "Command.h"

namespace battery {

Session::Session(Read r, Write w, Close c)
    : read(r), write(w), close(c) {

    }

Session::~Session() {

}

Command::Command(Session::Ptr session,
                 Encoder encoder,
                 Decoder decoder,
                 CompletionHandler handler)
  : session_(session),
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
  *hdr = Header();

  hdr->Len = sizeof(Header);

  char *request = (writeBuffer_ + sizeof(Header));
  char *response = (readBuffer_ + sizeof(Header));
  if(encoder_(request,
              BUFFER_SIZE, 
              hdr->Addr, 
              hdr->PackNo, 
              hdr->Cmd, 
              hdr->Len)) {
    // encode success, add tail section (checksum, EOI),
    Tail* tail = reinterpret_cast<Tail*>(writeBuffer_ + hdr->Len.value());
    *tail = Tail();

    tail->Chksum = checksum(writeBuffer_ + sizeof(Header::SOI),
                            hdr->Len.value() - sizeof(Header::SOI));

    // and deliver package!
    session_->write(writeBuffer_, (hdr->Len.value() + sizeof(Tail)), [=](
            const boost::system::error_code& ec,
            std::size_t bytes_transfered
            ) {
        // write completed, read response.
        readFrame();
        });
  } else {
    // encode failed, notify the requester.
    handler_(boost::system::errc::make_error_code(boost::system::errc::bad_message));
  }
}

void Command::readFrame() {

}

void Command::readComplete(const boost::system::error_code& ec,
		  std::size_t bytes_transfered) {

}

uint16_t Command::checksum(char* buff, std::size_t len) {
  uint16_t sum = 0;
  std::for_each(
      buff,
      buff + len,
      [&sum] (unsigned char c) {
      	  sum += c;
      });
  return sum;
}

} /* namespace battery */
