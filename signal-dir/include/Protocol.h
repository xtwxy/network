/*
 * Protocol.h
 *
 *  Created on: May 6, 2016
 *      Author: master
 */

#ifndef INCLUDE_PROTOCOL_H_
#define INCLUDE_PROTOCOL_H_

#include <boost/endian/buffers.hpp>

namespace dc2800_codec {

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

typedef boost::shared_ptr<AiData> AiDataPtr;
typedef boost::shared_ptr<ClockData> ClockDataPtr;

} /* namespace dc2800_codec */




#endif /* INCLUDE_PROTOCOL_H_ */
