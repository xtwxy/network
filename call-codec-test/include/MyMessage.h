/*
 * MyMessage.h
 *
 *  Created on: May 27, 2016
 *      Author: master
 */

#ifndef INCLUDE_MYMESSAGE_H_
#define INCLUDE_MYMESSAGE_H_

#include "CallProtocol.h"

struct MyMessage {
	MyMessage();
	uint32_t getAlarmId() const;
	void setAlarmId(uint32_t alarmId);
	double getCurrentValue() const;
	void setCurrentValue(double cv);
	uint8_t getStatus() const;
	void setStatus(uint8_t status);
	uint32_t getTimestamp() const;
	void setTimestamp(uint32_t timestamp);

	boost::endian::little_uint32_buf_t alarmId;
	boost::endian::little_uint32_buf_t timestamp;
	boost::endian::little_int8_buf_t status;
	boost::endian::big_uint64_buf_t currentValue;
	const static CallProtocol::MessageType TYPE_ID = 100;
};

#endif /* INCLUDE_MYMESSAGE_H_ */
