/*
 * Decoder.cpp
 *
 *  Created on: Apr 20, 2016
 *      Author: master
 */

#include <algorithm>
#include <boost/bind.hpp>

#include "../include/Codec.h"

namespace codec {


Session::Session(Read r, Write w, Close c) :
    read(r), write(w), close(c) {

    }

Session::~Session() {

}


FrameDecoder::FrameDecoder(SessionPtr session,
		CompletionHandler handler,
		std::size_t bufferSize)
: session_(session),
  handler_(handler),
  bytesRead_(0),
  BUFFER_SIZE(bufferSize),
  readBuffer_(new char[bufferSize]),
  bytesToRead_(sizeof(Header) + sizeof(Tail)) {
  reset();
}

void FrameDecoder::operator ()() {
  read();
}

void FrameDecoder::reset() {
	action_ = boost::bind(&FrameDecoder::decodeHeader,
	                        shared_from_this());
	bytesRead_ = 0;
	bytesToRead_ = (sizeof(Header) + sizeof(Tail));
}

void FrameDecoder::read() {
	session_->read(
      readBuffer_ + bytesRead_,
      bytesToRead_ - bytesRead_,
      boost::bind(&FrameDecoder::onReadComplete,
                  shared_from_this(),
                  _1,
                  _2)
			);
}

void FrameDecoder::decodeHeader() {
  Header* hdr = reinterpret_cast<Header*>(readBuffer_);
  do {
    if(hdr->valid()) {
      bytesToRead_ += sizeof(Header) + hdr->Len.value() + sizeof(Tail);
      action_ = boost::bind(&FrameDecoder::decodeToTail,
                            shared_from_this());
      break;
    } else {
      shiftReadBuffer(1);
    }
  } while(bytesRead_ >= sizeof(Header));

  if(bytesToRead_ == bytesRead_) {
    action_();
  } else {
    read();
  }
}

void FrameDecoder::decodeToTail() {
  // 1.check tail signature
	Header* hdr = reinterpret_cast<Header*>(readBuffer_);
	Tail* tail = reinterpret_cast<Tail*>(readBuffer_
			+ sizeof(Header)
			+ hdr->Len.value());

	boost::system::error_code bad_message =
		          boost::system::errc::make_error_code(
		              boost::system::errc::bad_message);
  if(!tail->valid()) {
	  handler_(bad_message, readBuffer_, bytesRead_);
	  reset();
	  return;
  }

  // 2.check checksum
  uint16_t chksum = checksum(readBuffer_ + sizeof(Header::SOI),
		sizeof(Header) - sizeof(Header::SOI) + hdr->Len.value());
  if(chksum != tail->Chksum.value()) {
	  handler_(bad_message, readBuffer_, bytesRead_);
	  reset();
	  return;
  }

  // 3.output to next stage decode
  boost::system::error_code success =
          boost::system::errc::make_error_code(
              boost::system::errc::success);
  handler_(success, readBuffer_, bytesRead_);
  reset();
}

void FrameDecoder::shiftReadBuffer(std::size_t offset) {
	std::move(readBuffer_ + offset,
			readBuffer_ + bytesRead_,
			readBuffer_
			);
	bytesRead_ -= offset;
}

FrameDecoder::~FrameDecoder() {
  delete[] readBuffer_;
}

void FrameDecoder::onReadComplete(
		const boost::system::error_code& ec,
		size_t bytes_transferred
	) {
	if(!ec) {
		bytesRead_ += bytes_transferred;
		if(bytesToRead_ == bytesRead_) {
			action_();
		} else {
			read();
		}
	} else {
		handler_(ec, readBuffer_, bytesRead_);
	}
}

uint16_t checksum(char* buff, std::size_t len) {
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
