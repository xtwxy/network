/*
 * Decoder.h
 *
 *  Created on: Apr 20, 2016
 *      Author: master
 */

#ifndef INCLUDE_FRAMECODEC_H_
#define INCLUDE_FRAMECODEC_H_

#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/noncopyable.hpp>
#include <boost/endian/buffers.hpp>
#include <boost/function.hpp>
#include <boost/system/error_code.hpp>

#include "Codec.h"

namespace dc2800_codec {

class FrameCodec : public boost::enable_shared_from_this<FrameCodec> {
public:
	typedef boost::shared_ptr<FrameCodec> Ptr;
	typedef boost::shared_ptr<FrameCodec> ContextPtr;
	FrameCodec();
	virtual ~FrameCodec();

	codec::CodecPtr getCodec();
	void encode(codec::Context&, boost::any&, std::list<boost::any>&);
	void decode(codec::Context&, boost::any&, std::list<boost::any>&);
	void sessionStart(codec::Context&);
	void sessionClose(codec::Context&);
	void exceptionCaught(codec::Context&, const std::exception&);
	static uint16_t checksum(char* buff, std::size_t len);
};

} /* namespace dc2800_codec */

#endif /* INCLUDE_FRAMECODEC_H_ */
