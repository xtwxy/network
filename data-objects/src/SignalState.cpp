/*
 * SignalState.cpp
 *
 *  Created on: May 31, 2016
 *      Author: master
 */

#include "SignalState.h"

using namespace std;
using namespace codec;
using namespace CallProtocol;

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

SignalType SignalState::getType() const {
  return signalType;
}

bool SignalState::expired() const {

}

bool SignalState::timeout() const {

}

void SignalState::addSubscriber(Correlation c, PipelinePtr pipeline) {
  subscribers.insert(std::make_pair(c, pipeline));
}

void SignalState::notifySubscribers() {
  StateChangeEventPtr event = boost::make_shared<StateChangeEvent>();
  event->setSource(shared_from_this());
  for(auto s = subscribers.begin(); s != subscribers.end(); ++s) {
    //TODO: Push StateChangeEvent to the subscriber.
    // Payload is a StateChangeEventPtr
    MessagePtr message = boost::make_shared<Message>(
        size(), 
        StateChangeEvent::TYPE_ID,
        s->first,
        event); 
    s->second->write(message);
  }
}

} /* namespace DataObjects */
