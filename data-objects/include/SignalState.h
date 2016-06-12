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
#include <list>
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
  const static CallProtocol::MessageType TYPE_ID = 3;
  static CallProtocol::PayloadPtr create();

  SignalId();
  SignalId(const std::string r);
  SignalId(const SignalId& r);
  virtual ~SignalId();

  SignalId& operator=(const SignalId& r);
  bool operator==(const SignalId& r) const;
  bool operator==(const std::string& r) const;
  bool operator==(const std::string r) const;
  bool operator<(const SignalId& r) const;

  const std::string& getValue() const;

  void load(boost::asio::streambuf&);
  void store(boost::asio::streambuf&) const;
  std::size_t size() const;
 private:
  std::string value;
};

class SignalState;
typedef boost::shared_ptr<SignalState> SignalStatePtr;

struct StateEvent : public CallProtocol::Payload {
 public:
  const static CallProtocol::MessageType TYPE_ID = 4;
  static CallProtocol::PayloadPtr create();

  StateEvent();
  StateEvent(const StateEvent&);
  StateEvent(const SignalStatePtr before, const SignalStatePtr after);

  StateEvent& operator=(const StateEvent&);

  void load(boost::asio::streambuf&);
  void store(boost::asio::streambuf&) const;
  std::size_t size() const;

  const SignalStatePtr getBefore() const;
  const SignalStatePtr getAfter() const;
 private:
  SignalStatePtr before;
  SignalStatePtr after;
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
  typedef boost::shared_ptr<SignalState> Ptr;
  virtual ~SignalState();

  SignalType getType() const;
  bool expired() const;
  bool timeout() const;
  const boost::posix_time::ptime& getTimestamp() const;

  virtual void load(boost::asio::streambuf&);
  virtual void store(boost::asio::streambuf&) const;
  virtual std::size_t size() const;
  static SignalStatePtr createFrom(boost::asio::streambuf&);
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
  virtual SignalStatePtr clone() const = 0;
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
  typedef boost::shared_ptr<AnalogState> Ptr;
  const static CallProtocol::MessageType TYPE_ID = 5;
  static CallProtocol::PayloadPtr create();

  AnalogState();
  AnalogState(const AnalogState&);
  virtual ~AnalogState();
  AnalogState& operator=(const AnalogState&);

  void setValue(double);
  double getValue() const;

  void load(boost::asio::streambuf&);
  void store(boost::asio::streambuf&) const;
  std::size_t size() const;
  SignalStatePtr clone() const;
 private:
  double value;
};

class BooleanState : public SignalState {
 public:
  typedef boost::shared_ptr<BooleanState> Ptr;
  const static CallProtocol::MessageType TYPE_ID = 6;
  static CallProtocol::PayloadPtr create();

  BooleanState();
  BooleanState(const BooleanState&);
  virtual ~BooleanState();
  BooleanState& operator=(const BooleanState&);

  void setValue(bool);
  bool getValue() const;

  void load(boost::asio::streambuf&);
  void store(boost::asio::streambuf&) const;
  std::size_t size() const;
  SignalStatePtr clone() const;
 private:
  bool value;
};

class StringState :public SignalState {
 public:
  typedef boost::shared_ptr<StringState> Ptr;
  const static CallProtocol::MessageType TYPE_ID = 7;
  static CallProtocol::PayloadPtr create();

  StringState();
  StringState(const StringState&);
  virtual ~StringState();
  StringState& operator=(const StringState&);

  void setValue(const std::string&);
  std::string getValue() const;

  void load(boost::asio::streambuf&);
  void store(boost::asio::streambuf&) const;
  std::size_t size() const;
  SignalStatePtr clone() const;
 private:
  std::string value;
};

class GetStateRequest : public CallProtocol::Payload {
 public:
  const static CallProtocol::MessageType TYPE_ID = 8;
  static CallProtocol::PayloadPtr create();

  typedef std::list<SignalId> SignalIds;
  GetStateRequest();
  GetStateRequest(const SignalIds&);
  GetStateRequest(const GetStateRequest&);
  virtual ~GetStateRequest();
  GetStateRequest& operator=(const GetStateRequest&);

  void addSignalId(const SignalId&);
  const SignalIds& getSignalIds() const;

  void load(boost::asio::streambuf&);
  void store(boost::asio::streambuf&) const;
  std::size_t size() const;
 private:
  SignalIds signalIds;
};

class GetStateResponse : public CallProtocol::Payload {
 public:
  const static CallProtocol::MessageType TYPE_ID = 9;
  static CallProtocol::PayloadPtr create();

  typedef std::map<SignalId, SignalStatePtr> Signals;
  GetStateResponse();
  GetStateResponse(const Signals&);
  GetStateResponse(const GetStateResponse&);
  virtual ~GetStateResponse();
  GetStateResponse& operator=(const GetStateResponse&);

  void addSignal(const SignalId&, const SignalStatePtr);
  const SignalStatePtr getSignal(const SignalId&) const;
  const Signals& getSignals() const;

  void load(boost::asio::streambuf&);
  void store(boost::asio::streambuf&) const;
  std::size_t size() const;
 private:
  Signals signals;
};

class SetStateRequest : public CallProtocol::Payload {
 public:
  const static CallProtocol::MessageType TYPE_ID = 10;
  static CallProtocol::PayloadPtr create();

  typedef std::map<SignalId, SignalStatePtr> Signals;
  SetStateRequest();
  SetStateRequest(const Signals&);
  SetStateRequest(const SetStateRequest&);
  virtual ~SetStateRequest();
  SetStateRequest& operator=(const SetStateRequest&);

  void addSignal(const SignalId&, const SignalStatePtr);
  const SignalStatePtr getSignal(const SignalId&) const;
  const Signals& getSignals() const;

  void load(boost::asio::streambuf&);
  void store(boost::asio::streambuf&) const;
  std::size_t size() const;
 private:
  Signals signals;
};

class SetStateResponse : public CallProtocol::Payload {
 public:
  const static CallProtocol::MessageType TYPE_ID = 11;
  static CallProtocol::PayloadPtr create();

  typedef uint8_t Result;
  typedef std::map<SignalId, Result> Results;
  SetStateResponse();
  SetStateResponse(const Results&);
  SetStateResponse(const SetStateResponse&);
  virtual ~SetStateResponse();
  SetStateResponse& operator=(const SetStateResponse&);

  void addResult(const SignalId&, const Result);
  const Result getResult(const SignalId&) const;
  const Results& getResults() const;

  void load(boost::asio::streambuf&);
  void store(boost::asio::streambuf&) const;
  std::size_t size() const;
 private:
  Results results;
};

void PayloadFactoryInitializer(CallProtocol::PayloadFactory& );

} /* namespace DataObjects */

#endif /* INCLUDE_SIGNALSTATE_H_ */
