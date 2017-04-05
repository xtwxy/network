#ifndef _CODEC_BYTE_STREAM_H
#define _CODEC_BYTE_STREAM_H

#include "completion_handler.hpp"

class ByteStream {
 public:
	typedef boost::asio::streambuf::mutable_buffers_type ReadBuffer;
	typedef boost::asio::streambuf::const_buffers_type WriteBuffer;
	typedef boost::function<
			void (const boost::system::error_code&, std::size_t)> IoCompHandler;
  
  virtual ~ByteStream() = 0;

  virtual void read(ReadBuffer, IoCompHandler);
  virtual void write(WriteBuffer, IoCompHandler);

  virtual void setReadTimeout(unsigned long millsecs,
};

#endif // _CODEC_BYTE_STREAM_H
