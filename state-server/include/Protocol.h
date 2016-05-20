/*
 * Protocol.h
 *
 *  Created on: May 20, 2016
 *      Author: master
 */

#ifndef INCLUDE_PROTOCOL_H_
#define INCLUDE_PROTOCOL_H_
#include <boost/endian/buffers.hpp>  // see Synopsis below
#include <boost/static_assert.hpp>

namespace state_server {

class Protocol {
public:
	Protocol();
	virtual ~Protocol();
};




} /* namespace state_server */

#endif /* INCLUDE_PROTOCOL_H_ */
