/*
 * SignalState.cpp
 *
 *  Created on: May 31, 2016
 *      Author: master
 */

#include "SignalState.h"

namespace DataObjects {

SignalId::SignalId() { }

SignalId::SignalId(const std::string r) { }

SignalId::SignalId(const SignalId& r) { }

SignalId::~SignalId() { }

SignalId& SignalId::operator=(const SignalId& r) {
	this->value = r.value;
	return *this;
}

bool SignalId::operator==(const SignalId& r) const {
	return (this->value == r.value);
}

bool SignalId::operator<(const SignalId& r) const {
	return (this->value < r.value);
}

SignalState::SignalState() {

}

SignalState::~SignalState() {

}

} /* namespace DataObjects */
