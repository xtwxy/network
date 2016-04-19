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
#include "nw/Acceptor.hpp"

namespace battery {

class Command : public boost::enable_shared_from_this<Command>,
public boost::noncopyable {
 public:
  typedef boost::shared_ptr<Command> Ptr;

  typedef boost::function<
      bool (char * const,                       // Output buffer
            std::size_t,                       // Length of bytes encoded
            boost::endian::little_uint8_buf_t,  // Addr
            boost::endian::little_uint8_buf_t,  // PackNo
            boost::endian::little_uint8_buf_t,  // Cmd
            boost::endian::little_uint16_buf_t  // Len
           )> Decoder;
  
  typedef boost::function<
      bool (char * const,                       // Output buffer
            std::size_t,                       // Length of bytes encoded
            boost::endian::little_uint8_buf_t,  // Addr
            boost::endian::little_uint8_buf_t,  // PackNo
            boost::endian::little_uint8_buf_t,  // Cmd
            boost::endian::little_uint16_buf_t  // Len
           )> Encoder;
  typedef boost::function<void (const boost::system::error_code&)> CompletionHandler;
  
  Command(nw::Connection::Ptr conn,
          Encoder encoder,
          Decoder decoder,
          CompletionHandler handler);
  
  virtual ~Command();

  void execute();

 private:
  uint16_t checksum(char* buff, std::size_t len);
  struct Header {
    Header() {
      SOH[0] = 0xfa;
      SOH[1] = 0x55;
      SOH[2] = 0xfa;
      SOH[3] = 0x55;
      SOH[4] = 0xfa;
      SOH[5] = 0x55;
    }
    boost::endian::little_uint8_buf_t SOH[4];
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
    boost::endian::little_uint16_buf_t Chksum;
    boost::endian::little_uint8_buf_t EOI[4];
  };

  nw::Connection::Ptr conn_;
  Encoder encoder_;
  Decoder decoder_;
  CompletionHandler handler_;
  const std::size_t BUFFER_SIZE;
  char * readBuffer_;
  char * writeBuffer_;
};

} /* namespace battery */

#endif /* INCLUDE_COMMAND_H_ */
