/*
 * Decoder.cpp
 *
 *  Created on: Apr 20, 2016
 *      Author: master
 */

#include <algorithm>
#include <boost/bind.hpp>
#include <boost/make_shared.hpp>

#include "Protocol.h"
#include "FrameCodec.h"

using namespace codec;
using namespace std;

namespace dc2800_codec {


uint16_t FrameCodec::checksum(char* buff, std::size_t len) {
  uint16_t sum = 0;
  std::for_each(
      buff,
      buff + len,
      [&sum] (unsigned char c) {
      	  sum += c;
      });
  return sum;
}

FrameCodec::FrameCodec() {}
FrameCodec::~FrameCodec() { }

codec::CodecPtr FrameCodec::getCodec() {
	Ptr codec = boost::make_shared<FrameCodec>();
	EncodeFunc encoder = boost::bind(&FrameCodec::encode, codec, _1, _2,	_3);
	EncodeFunc decoder = boost::bind(&FrameCodec::decode, codec, _1, _2,	_3);

	SessionStart ssnStart = boost::bind(&FrameCodec::sessionStart, codec, _1);
	SessionClose ssnClose = boost::bind(&FrameCodec::sessionClose, codec, _1);

	ExceptionCaught exceptCaught = boost::bind(&FrameCodec::exceptionCaught, codec, _1, _2);

	CodecPtr ptr(new Codec(encoder, decoder, ssnStart, ssnClose, exceptCaught));

	return ptr;

}

void FrameCodec::encode(codec::Context& ctx, boost::any& input, std::list<boost::any>& output) {
	output.push_back(input);
}

void FrameCodec::decode(codec::Context& ctx, boost::any& input, std::list<boost::any>& output) {
	output.push_back(input);
}

void FrameCodec::sessionStart(codec::Context& ctx) {

}

void FrameCodec::sessionClose(codec::Context& ctx) {

}

void FrameCodec::exceptionCaught(codec::Context& ctx, const std::exception& ex) {

}


} /* namespace dc2800_codec */
