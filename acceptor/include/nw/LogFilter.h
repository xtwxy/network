/*
 * LogFilter.h
 *
 *  Created on: Mar 22, 2016
 *      Author: master
 */

#ifndef INCLUDE_NW_LOGFILTER_H_
#define INCLUDE_NW_LOGFILTER_H_

#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

namespace nw {

class LogFilter : public boost::enable_shared_from_this<LogFilter> {
public:
	typedef boost::shared_ptr<LogFilter> Ptr;
	LogFilter();
	virtual ~LogFilter();

};

} /* namespace nw */

#endif /* INCLUDE_NW_LOGFILTER_H_ */
