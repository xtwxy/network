/*
 * CodecFactory.h
 *
 *  Created on: Mar 22, 2016
 *      Author: master
 */

#ifndef INCLUDE_NW_CODECFACTORY_H_
#define INCLUDE_NW_CODECFACTORY_H_

#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

namespace nw {

class CodecFactory :public boost::enable_shared_from_this<CodecFactory> {
public:
	typedef boost::shared_ptr<CodecFactory> Ptr;
	CodecFactory();
	virtual ~CodecFactory();
};

} /* namespace nw */

#endif /* INCLUDE_NW_CODECFACTORY_H_ */
