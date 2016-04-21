/*
 * Decoder.h
 *
 *  Created on: Apr 20, 2016
 *      Author: master
 */

#ifndef INCLUDE_DECODER_H_
#define INCLUDE_DECODER_H_

#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/noncopyable.hpp>
#include <boost/endian/buffers.hpp>
#include <boost/function.hpp>
#include <boost/system/error_code.hpp>

#include "Command.h"

namespace battery {

class Session;

uint16_t checksum(char* buff, std::size_t len);

class Decoder : public boost::enable_shared_from_this<Decoder>,
public boost::noncopyable {
public:
	typedef boost::shared_ptr<Decoder> Ptr;
	typedef boost::function<
			void (const boost::system::error_code&,
					char*,
					std::size_t)> CompletionHandler;
	typedef boost::shared_ptr<Session> SessionPtr;

	Decoder(SessionPtr session,
			CompletionHandler handler,
			std::size_t bufferSize = 512);
	virtual ~Decoder();

	void operator()();

private:
  void decode();
  void decodeHeader();
  void decodeToTail();
  void shiftReadBuffer(std::size_t offset);
  void read();
  void onReadComplete(
			const boost::system::error_code& ec,
			size_t bytes_transferred
		);

	SessionPtr session_;
	CompletionHandler handler_;
	std::size_t bytesRead_;
	const std::size_t BUFFER_SIZE;
	char* readBuffer_;
  std::size_t bytesToRead_;
  
  boost::function<void ()> action_;
};

} /* namespace battery */

#endif /* INCLUDE_DECODER_H_ */
