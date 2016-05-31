/*
 * SignalState.h
 *
 *  Created on: May 31, 2016
 *      Author: master
 */

#ifndef INCLUDE_SIGNALSTATE_H_
#define INCLUDE_SIGNALSTATE_H_

#include "CallProtocol.h"

namespace DataObjects {

enum SignalType { AI=1, DI=2, SI=3, AO=4, DO=8, SO=12 };

struct SequenceType {
	boost::endian::little_uint32_buf_t length;
};
template<std::size_t N>
struct StringValue {
	StringValue() : length(N) { }
	boost::endian::little_uint32_buf_t length;
	boost::endian::little_uint8_buf_t value[N];
};
struct SignalState {
	SignalState();
	~SignalState();

  boost::endian::little_uint8_buf_t type;
  union value {
    boost::endian::little_uint8_buf_t boolValue;
    boost::endian::big_uint64_buf_t analogValue;
    template<std::size_t N>
    StringValue<N> stringValue;
  };
};

} /* namespace DataObjects */

#endif /* INCLUDE_SIGNALSTATE_H_ */
