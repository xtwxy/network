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
  BUFFER_SIZE(bufferSize) {

}

void Decoder::operator ()() {
	session_->read(

			);

}

Decoder::~Decoder() {

}

void Decoder::onReadComplete(
		const boost::system::error_code& ec,
		size_t bytes_transferred
	) {
	if(!ec) {
		bytesRead_ += bytes_transferred;
	} else {
	}
}


} /* namespace battery */
