/*
 * DataHandler.h
 *
 *  Created on: May 6, 2016
 *      Author: master
 */

#ifndef INCLUDE_DATAHANDLER_H_
#define INCLUDE_DATAHANDLER_H_

#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/noncopyable.hpp>

#include "Codec.h"

namespace dc2800_codec {

class DataHandler : public boost::enable_shared_from_this<DataHandler>,
private boost::noncopyable {
public:
	typedef boost::shared_ptr<DataHandler> Ptr;
	DataHandler();
	virtual ~DataHandler();

	void handle(codec::Context&,	boost::any&);
	void sessionStart(codec::Context&);
	void sessionClose(codec::Context&);
	void exceptionCaught(codec::Context&, const std::exception&);
	codec::HandlerPtr getHandler();
};

} /* namespace dc2800_codec */

#endif /* INCLUDE_DATAHANDLER_H_ */
