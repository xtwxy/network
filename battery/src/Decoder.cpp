/*
 * Decoder.cpp
 *
 *  Created on: Apr 20, 2016
 *      Author: master
 */

#include "Decoder.h"

namespace battery {

Decoder::Decoder(SessionPtr session,
		CompletionHandler handler,
		std::size_t bufferSize)
: session_(session),
  handler_(handler),
  bytesRead_(0),
  BUFFER_SIZE(bufferSize),
  readBuffer_(new char[bufferSize]),
  bytesToRead_(sizeof(Header) + sizeof(Tail)) {
  action_ = boost::bind(&Decoder::decodeHeader, 
                        shared_from_this());
}

void Decoder::operator ()() {
  read();
}

void Decoder::read() {
	session_->read(
      readBuffer_ + bytesRead_,
      bytesToRead_ - bytesRead_,
      boost::bind(&Decoder::onReadComplete,
                  shared_from_this(),
                  _1,
                  _2)
			);
}

void Decoder::decodeHeader() {
  Header* hdr = reinterpret_cast<Header*>(readBuffer_);
  do {
    if(hdr->valid()) {
      bytesToRead_ += hdr->Len.value() + sizeof(Tail);
      action_ = boost::bind(&Decoder::decodeToTail,
                            shared_from_this());
      break;
    } else {
      shiftReadBuffer(1);
    }
  } while(bytesRead >= sizeof(Header));

  if(bytesToRead_ == bytesRead_) {
    action_();
  } else {
    read();
  }
}

void Decoder::decodeToTail() {
  // 1.check tail signature
  if() {

  } else {

  }
  // 2.check checksum
  // 3.output to next stage decode
}


Decoder::~Decoder() {
  delete[] readBuffer_;
}

void Decoder::onReadComplete(
		const boost::system::error_code& ec,
		size_t bytes_transferred
	) {
	if(!ec) {
		bytesRead_ += bytes_transferred;
    if(bytesToRead_ == bytesRead) {
      action_();
    } else {
      read();
    }
	} else {
    handler_(ec, readBuffer_, bytesRead_);
	}
}


} /* namespace battery */
