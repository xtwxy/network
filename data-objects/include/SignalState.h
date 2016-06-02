/*
 * SignalState.h
 *
 *  Created on: May 31, 2016
 *      Author: master
 */

#ifndef INCLUDE_SIGNALSTATE_H_
#define INCLUDE_SIGNALSTATE_H_

#include <ctime>
#include <boost/date_time/local_time/local_time.hpp>

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

class SignalValue : public CallProtocol::Payload {
public:
	SignalValue();
	virtual ~SignalValue();

	SignalType getType() const;
	bool expired() const;
	bool timeout() const;

	void addSubscriber(codec::PipelinePtr);
protected:
	void notifySubscriber();
private:
	const SignalType signalType;
	const time_t timeoutSeconds;
	const time_t expireSeconds;
	boost::posix_time::ptime timestamp;
	std::vector<codec::PipelinePtr> subscribers;
};

class AnalogValue : public SignalValue {
public:
	AnalogValue();
	virtual ~AnalogValue();

	void setValue(double);
	double getValue() const;

	void load(std::streambuf&);
	void store(std::streambuf&);
	std::size_t size();
private:
	double value;
};

class BooleanValue : public SignalValue {
public:
	BooleanValue();
	virtual ~BooleanValue();

	void setValue(bool);
	bool getValue() const;

	void load(std::streambuf&);
	void store(std::streambuf&);
	std::size_t size();
private:
	bool value;
};

class StringValue :public CallProtocol::Payload {
public:
	StringValue();

	void setValue(const std::string&);
	const std::string& getValue() const;

	void load(std::streambuf&);
	void store(std::streambuf&);
	std::size_t size();
private:
	std::string value;
};

class SignalState {
public:
	SignalState();
	~SignalState();
private:

};

} /* namespace DataObjects */

#endif /* INCLUDE_SIGNALSTATE_H_ */
