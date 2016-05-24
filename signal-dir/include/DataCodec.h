/*
 * DataCodec.h
 *
 *  Created on: May 6, 2016
 *      Author: master
 */

#ifndef INCLUDE_DATACODEC_H_
#define INCLUDE_DATACODEC_H_

#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/noncopyable.hpp>

#include "Codec.h"

namespace dc2800_codec {

class DataCodec : public boost::enable_shared_from_this<DataCodec> {
public:
	typedef boost::shared_ptr<DataCodec> Ptr;
	typedef boost::shared_ptr<DataCodec> ContextPtr;
	DataCodec();
	virtual ~DataCodec();

	codec::CodecPtr getCodec();
	void encode(codec::Context&, boost::any&, std::list<boost::any>&);
	void decode(codec::Context&, boost::any&, std::list<boost::any>&);
	void sessionStart(codec::Context&);
	void sessionClose(codec::Context&);
	void exceptionCaught(codec::Context&, const std::exception&);
};

} /* namespace dc2800_codec */

#endif /* INCLUDE_DATACODEC_H_ */
