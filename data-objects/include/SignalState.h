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

enum SignalType { AI=1, DI=2, SI=3, AO=4, DO=8, SO=12 };

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

class SignalState : public CallProtocol::Payload,
boost::noncopyable {
public:
	SignalState();
	virtual ~SignalState();

	SignalType getType() const;
	bool expired() const;
	bool timeout() const;

	void addSubscriber(CallProtocal::Correlation, codec::PipelinePtr);
protected:
	void notifySubscribers();
private:
	const SignalType signalType;
	const time_t timeoutSeconds;
	const time_t expireSeconds;
	boost::posix_time::ptime timestamp;
	std::map<CallProtocol::Correlation, codec::PipelinePtr> subscribers;
};

class AnalogState : public SignalState {
public:
	AnalogState();
	virtual ~AnalogState();

	void setValue(double);
	double getValue() const;

	void load(std::streambuf&);
	void store(std::streambuf&);
	std::size_t size();
private:
	double value;
};

class BooleanState : public SignalState {
public:
	BooleanState();
	virtual ~BooleanState();

	void setValue(bool);
	bool getValue() const;

	void load(std::streambuf&);
	void store(std::streambuf&);
	std::size_t size();
private:
	bool value;
};

class StringState :public CallProtocol::Payload {
public:
	StringState();

	void setValue(const std::string&);
	const std::string& getValue() const;

	void load(std::streambuf&);
	void store(std::streambuf&);
	std::size_t size();
private:
	std::string value;
};

} /* namespace DataObjects */

#endif /* INCLUDE_SIGNALSTATE_H_ */
