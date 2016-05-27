/*
 * MyMessage.cpp
 *
 *  Created on: May 27, 2016
 *      Author: master
 */

#include "MyMessage.h"

MyMessage::MyMessage() :
		alarmId(), timestamp(), status(), currentValue() {
}
uint32_t MyMessage::getAlarmId() const {
	return alarmId.value();
}

void MyMessage::setAlarmId(uint32_t alarmId) {
	this->alarmId = alarmId;
}

double MyMessage::getCurrentValue() const {
	uint64_t v = currentValue.value();
	return *reinterpret_cast<double*>(&v);
}

void MyMessage::setCurrentValue(double cv) {
	uint64_t v = *reinterpret_cast<unsigned long int*>(&cv);
	this->currentValue = v;
}

uint8_t MyMessage::getStatus() const {
	return status.value();
}

void MyMessage::setStatus(uint8_t status) {
	this->status = status;
}

uint32_t MyMessage::getTimestamp() const {
	return timestamp.value();
}

void MyMessage::setTimestamp(uint32_t timestamp) {
	this->timestamp = timestamp;
}
