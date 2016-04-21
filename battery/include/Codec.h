/*
 * Decoder.h
 *
 *  Created on: Apr 20, 2016
 *      Author: master
 */

#ifndef INCLUDE_CODEC_H_
#define INCLUDE_CODEC_H_

#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/noncopyable.hpp>
#include <boost/endian/buffers.hpp>
#include <boost/function.hpp>
#include <boost/system/error_code.hpp>

#include "Command.h"

namespace battery {

struct Header {
  Header() {
    SOI[0] = 0xfa;
    SOI[1] = 0x55;
    SOI[2] = 0xfa;
    SOI[3] = 0x55;
    SOI[4] = 0xfa;
    SOI[5] = 0x55;
  }
  bool valid() {
    if(SOI[0].value() != 0xfa) return false;
    if(SOI[1].value() != 0x55) return false;
    if(SOI[2].value() != 0xfa) return false;
    if(SOI[3].value() != 0x55) return false;
    if(SOI[4].value() != 0xfa) return false;
    if(SOI[5].value() != 0x55) return false;

    if(Addr.value() > 8) return false;
    if(PackNo.value() > 8) return false;
    if(Cmd.value() > 8) return false;
    if(Len.value() > 132) return false;

    return true;
  }

  boost::endian::little_uint8_buf_t SOI[6];
  boost::endian::little_uint8_buf_t Addr;
  boost::endian::little_uint8_buf_t PackNo;
  boost::endian::little_uint8_buf_t Cmd;
  boost::endian::little_uint16_buf_t Len;
};

struct Tail {
  Tail() {
    EOI[0] = 0xfd;
    EOI[1] = 0x22;
    EOI[2] = 0xfd;
    EOI[3] = 0x22;
  }
  bool valid() {
    if(EOI[0].value() != 0xfd) return false;
    if(EOI[1].value() != 0x22) return false;
    if(EOI[2].value() != 0xfd) return false;
    if(EOI[3].value() != 0x22) return false;

    return true;
  }

  boost::endian::little_uint16_buf_t Chksum;
  boost::endian::little_uint8_buf_t EOI[4];
};

struct AiData {
	  boost::endian::little_uint32_buf_t cellVolts[24];
	  boost::endian::little_uint32_buf_t batteryVolts;
	  boost::endian::little_uint32_buf_t batteryCurrent;
	  boost::endian::little_uint32_buf_t temperature[4];
};

struct ClockData {
	  boost::endian::little_uint32_buf_t secondsFromEpoch;
};


class Session: public boost::enable_shared_from_this<Session>,
		public boost::noncopyable {
public:
	typedef boost::shared_ptr<Session> Ptr;
	typedef boost::function<void (const boost::system::error_code&, std::size_t)> Handler;
	typedef boost::function<void (char*, std::size_t, Handler)> Read;
	typedef boost::function<void (char*, std::size_t, Handler)> Write;
	typedef boost::function<void ()> Close;

	Session(Read r, Write w, Close c);

	virtual ~Session();

	Read read;
	Write write;
	Close close;
};


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
  void reset();
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

#endif /* INCLUDE_CODEC_H_ */
