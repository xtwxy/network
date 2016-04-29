/*
 * Codec.h
 *
 *  Created on: Mar 22, 2016
 *      Author: master
 */

#ifndef INCLUDE_NW_CODEC_H_
#define INCLUDE_NW_CODEC_H_

#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

namespace nw {

class Codec :public boost::enable_shared_from_this<Codec> {
public:
	typedef boost::shared_ptr<Codec> Ptr;
	Codec();
	virtual ~Codec();

	//virtual std::size_t encode(char *buffer, std::size_t offset, std::size_t len, )
};

} /* namespace nw */

#endif /* INCLUDE_NW_CODEC_H_ */
