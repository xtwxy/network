/*
 * SignalState.h
 *
 *  Created on: May 31, 2016
 *      Author: master
 */

#ifndef INCLUDE_SIGNALSTATE_H_
#define INCLUDE_SIGNALSTATE_H_

#include <ctime>
#include <map>
#include <boost/date_time/local_time/local_time.hpp>
#include <boost/noncopyable.hpp>

#include "CallProtocol.h"

namespace DataObjects {

const uint8_t AI=1;
const uint8_t DI=2;
const uint8_t SI=3;

typedef uint8_t SignalType;

const static time_t TIMEOUT_SECONDS = 5;
const static time_t EXPIRE_SECONDS = 30;

class SignalId : public CallProtocol::Payload {
public:
	SignalId();
	SignalId(const std::string r);
	SignalId(const SignalId& r);
	virtual ~SignalId();

	SignalId& operator=(const SignalId& r);
	bool operator==(const SignalId& r) const;
	bool operator<(const SignalId& r) const;
private:
	const std::string value;
};

class SignalState;
typedef boost::shared_ptr<SignalState> SignalStatePtr;

struct StateEvent : public CallProtocol::Payload {
public:
	StateEvent(const StateEvent&);
	StateEvent(const SignalStatePtr before, const SignalStatePtr after);

	StateEvent& operator=(const StateEvent&);

	void load(std::streambuf&);
	void store(std::streambuf&);
	std::size_t size();

  const SignalStatePtr getBefore() const;
  const SignalStatePtr getAfter() const;
private:
  const SignalStatePtr before;
	const SignalStatePtr after;
};

typedef boost::shared_ptr<StateEvent> StateEventPtr;

class StateListener : private boost::noncopyable { 
public:
	StateListener();
	virtual ~StateListener();

	virtual void stateChanged(StateEventPtr) = 0;
};

typedef boost::shared_ptr<StateListener> StateListenerPtr;

class SignalState : public CallProtocol::Payload {
public:
	virtual ~SignalState();

	SignalType getType() const;
	bool expired() const;
	bool timeout() const;
  const boost::posix_time::ptime& getTimestamp() const;

	virtual void load(std::streambuf&);
	virtual void store(std::streambuf&);
	virtual std::size_t size();
	
	void addChangeListener(StateListenerPtr);
protected:
	SignalState(const SignalType signalType,
			const time_t timeoutSeconds,
			const time_t expireSeconds,
			const boost::posix_time::ptime& timestamp);
	SignalState(const SignalType signalType,
			const time_t timeoutSeconds,
			const time_t expireSeconds,
			const boost::posix_time::ptime& timestamp,
			const std::vector<StateListenerPtr>& listeners);
	SignalState(const SignalState&);
	SignalState& operator=(const SignalState&);

  void fireStateChange(SignalStatePtr before, SignalStatePtr after);
	virtual SignalStatePtr clone() = 0;
  void updateTimestamp();
private:
  // remove const for signal type, for load()
  	SignalType signalType;
	const time_t timeoutSeconds;
	const time_t expireSeconds;
	boost::posix_time::ptime timestamp;
	std::vector<StateListenerPtr> listeners;
};

class AnalogState : public SignalState {
public:
	AnalogState();
	AnalogState(const AnalogState&);
	virtual ~AnalogState();
	AnalogState& operator=(const AnalogState&);

	void setValue(double);
	double getValue() const;

	void load(std::streambuf&);
	void store(std::streambuf&);
	std::size_t size();
	SignalStatePtr clone();
private:
	double value;
};

class BooleanState : public SignalState {
public:
	BooleanState();
	BooleanState(const BooleanState&);
	virtual ~BooleanState();
	BooleanState& operator=(const BooleanState&);

	void setValue(bool);
	bool getValue() const;

	void load(std::streambuf&);
	void store(std::streambuf&);
	std::size_t size();
	SignalStatePtr clone();
private:
	bool value;
};

class StringState :public SignalState {
public:
	StringState();
	StringState(const StringState&);
	virtual ~StringState();
	StringState& operator=(const StringState&);

	void setValue(const std::string&);
	std::string getValue() const;

	void load(std::streambuf&);
	void store(std::streambuf&);
	std::size_t size();
	SignalStatePtr clone();
private:
	std::string value;
};

} /* namespace DataObjects */

#endif /* INCLUDE_SIGNALSTATE_H_ */
