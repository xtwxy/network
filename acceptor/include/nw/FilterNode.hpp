/*
 * FilterNode.hpp
 *
 *  Created on: Mar 22, 2016
 *      Author: master
 */

#ifndef INCLUDE_NW_FILTERNODE_HPP_
#define INCLUDE_NW_FILTERNODE_HPP_

#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <nw/LogFilter.h>
#include <nw/CodecFactory.h>

namespace nw {

class FilterNode :public boost::enable_shared_from_this<FilterNode> {
public:
	typedef boost::shared_ptr<FilterNode> Ptr;
	FilterNode() { }
	virtual ~FilterNode() { }

private:
	LogFilter::Ptr logFilter_;
	CodecFactory::Ptr codecFactory_;
};

} /* namespace nw */

#endif /* INCLUDE_NW_FILTERNODE_HPP_ */
