/*
 * Command.h
 *
 *  Created on: Apr 19, 2016
 *      Author: master
 */

#ifndef INCLUDE_COMMAND_H_
#define INCLUDE_COMMAND_H_

#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/noncopyable.hpp>
#include <boost/endian/buffers.hpp>
#include <boost/function.hpp>
#include <boost/system/error_code.hpp>

#include "Codec.h"

namespace codec {

class Session;

class Command : public boost::enable_shared_from_this<Command>,
public boost::noncopyable {
 public:
  typedef boost::shared_ptr<Command> Ptr;
  typedef boost::shared_ptr<Session> SessionPtr;

  typedef boost::function<
      bool (char * const,                        // Input buffer
            std::size_t,                         // Length of bytes encoded
            boost::endian::little_uint8_buf_t&,  // Addr
            boost::endian::little_uint8_buf_t&,  // PackNo
            boost::endian::little_uint8_buf_t&,  // Cmd
            boost::endian::little_uint16_buf_t&  // Len
           )> DataDecoder;
  
  typedef boost::function<
      bool (char * const,                        // Output buffer
            std::size_t,                         // Length of bytes encoded
            boost::endian::little_uint8_buf_t&,  // Addr
            boost::endian::little_uint8_buf_t&,  // PackNo
            boost::endian::little_uint8_buf_t&,  // Cmd
            boost::endian::little_uint16_buf_t&  // Len
           )> DataEncoder;
  typedef boost::function<void (const boost::system::error_code&)> CompletionHandler;
  
  Command(SessionPtr session,
          FrameEncoder frameEncoder,
          DataEncoder dataEncoder,
          FrameDecoder frameDecoder,
          DataDecoder datadecoder,
          CompletionHandler handler);
  
  virtual ~Command();

  void execute();

 private:

  SessionPtr session;
  FrameEncoder encodeFrame;
  DataEncoder encodeData;
  FrameDecoder decodeFrame;
  DataDecoder decodeData;
  CompletionHandler complete;
};

} /* namespace battery */

#endif /* INCLUDE_COMMAND_H_ */
