/*
 * SignalState.cpp
 *
 *  Created on: May 31, 2016
 *      Author: master
 */
#include <boost/bind.hpp>
#include "SignalState.h"

using namespace std;
using namespace codec;
using namespace CallProtocol;

namespace DataObjects {

CallProtocol::PayloadPtr SignalId::create() {
	return boost::make_shared<SignalId>();
}

SignalId::SignalId() { }

SignalId::SignalId(const std::string r) 
    : value(r) { 

}

SignalId::SignalId(const SignalId& r)
    : value(r.value) { 

}

SignalId::~SignalId() { 

}

SignalId& SignalId::operator=(const SignalId& r) {
  return *this;
}

bool SignalId::operator==(const SignalId& r) const {
  return (this->value == r.value);
}

bool SignalId::operator==(const std::string& r) const {
  return (this->value == r);
}

bool SignalId::operator==(const std::string r) const {
  return (this->value == r);
}

const std::string& SignalId::getValue() const {
  return value;
}

bool SignalId::operator<(const SignalId& r) const {
  return (this->value < r.value);
}

void SignalId::load(boost::asio::streambuf& sb) {
  uint32_t len;
  boost::endian::little_uint32_buf_t lenRepr;
  sb.sgetn(reinterpret_cast<char*>(&lenRepr), sizeof(lenRepr));
  len = *reinterpret_cast<uint32_t*>(&lenRepr);
  for(size_t i = 0; i != len; ++i) {
    char c = sb.sbumpc();
    value.push_back(c);
  }
}

void SignalId::store(boost::asio::streambuf& sb) const {
  uint32_t len = value.length();
  boost::endian::little_uint32_buf_t lenRepr;
  lenRepr = len;
  sb.sputn(lenRepr.data(), sizeof(lenRepr));
  for(auto& c : value) {
    sb.sputc(c);
  }
}

std::size_t SignalId::size() const {
  return (sizeof(uint32_t) + this->value.size());
}

CallProtocol::PayloadPtr StateEvent::create() {
	return boost::make_shared<StateEvent>();
}

StateEvent::StateEvent()
  : before(),
    after() {
}

StateEvent::StateEvent(const StateEvent& r) 
  : before(r.before),
    after(r.after) {
}

StateEvent::StateEvent(const SignalStatePtr before, const SignalStatePtr after) 
  : before(before), after(after) {

}

StateEvent& StateEvent::operator=(const StateEvent& r) {
  return *this;
}

void StateEvent::load(boost::asio::streambuf& sb) {
  before = SignalState::createFrom(sb);
  after = SignalState::createFrom(sb);
}

void StateEvent::store(boost::asio::streambuf& sb) const {
  before->store(sb);
  after->store(sb);
}

std::size_t StateEvent::size() const {
  return (before->size() + after->size());
}

const SignalStatePtr StateEvent::getBefore() const {
  return before;
}

const SignalStatePtr StateEvent::getAfter() const {
  return after;
}

StateListener::StateListener() {

}

StateListener::~StateListener() {

}

SignalState::SignalState(const SignalType signalType,
                         const time_t timeoutSeconds,
                         const time_t expireSeconds,
                         const boost::posix_time::ptime& timestamp)
  : signalType(signalType),
    timeoutSeconds(timeoutSeconds),
    expireSeconds(expireSeconds),
    timestamp(timestamp),
    listeners() {

}

SignalState::SignalState(const SignalType signalType,
                         const time_t timeoutSeconds,
                         const time_t expireSeconds,
                         const boost::posix_time::ptime& timestamp,
                         const std::vector<StateListenerPtr>& listeners)
  : signalType(signalType),
    timeoutSeconds(timeoutSeconds),
    expireSeconds(expireSeconds),
    timestamp(timestamp),
    listeners(listeners) {

}

SignalState::~SignalState() {

}

SignalState::SignalState(const SignalState& r) 
  : signalType(r.signalType),
    timeoutSeconds(r.timeoutSeconds),
    expireSeconds(r.expireSeconds),
    timestamp(r.timestamp),
    listeners(r.listeners) {
}

SignalState& SignalState::operator=(const SignalState& r) {
  this->timestamp = r.timestamp;
  this->listeners = r.listeners;

  return *this;
}

SignalType SignalState::getType() const {
  return signalType;
}

void SignalState::load(boost::asio::streambuf& sb) {
  signalType = sb.sbumpc();
  // transient: const time_t timeoutSeconds;
  // transient: const time_t expireSeconds;
  boost::endian::little_int64_buf_t secsRepr;
  sb.sgetn(reinterpret_cast<char*>(&secsRepr), sizeof(secsRepr));
  time_t s = secsRepr.value();
  if(s == 0) {
    timestamp = boost::posix_time::ptime();
  } else {
    timestamp = boost::posix_time::from_time_t(s);
  }
  // transient: std::vector<StateListenerPtr> listeners;
}

void SignalState::store(boost::asio::streambuf& sb) const {
  sb.sputc(signalType);
  boost::endian::little_int64_buf_t secsRepr;
  try{
    tm t = to_tm(timestamp);
    time_t secs = mktime(&t);
    secsRepr = secs;
  } catch(...) {
    secsRepr = 0;
  }
  sb.sputn(reinterpret_cast<char*>(&secsRepr), sizeof(secsRepr));
}

SignalStatePtr SignalState::createFrom(boost::asio::streambuf& sb) {
  SignalStatePtr ptr;
  SignalType type = sb.sgetc();
  switch(type) {
    case DI:
      ptr.reset(new BooleanState());
      ptr->load(sb);
      break;
    case AI:
      ptr.reset(new AnalogState());
      ptr->load(sb);
      break;
    case SI:
      ptr.reset(new StringState());
      ptr->load(sb);
      break;
    default:
      throw std::invalid_argument("Invalid signal type: ");
      break;
  }
  return ptr;
}

std::size_t SignalState::size() const {
  // TODO: implement SignalState::size();
  return sizeof(SignalType) + sizeof(boost::endian::little_int64_buf_t);
}

bool SignalState::expired() const {
  boost::posix_time::ptime ts = boost::posix_time::second_clock::local_time();
  boost::posix_time::time_duration td = ts - timestamp;
  return (td.total_seconds() > expireSeconds);
}

bool SignalState::timeout() const {
  boost::posix_time::ptime ts = boost::posix_time::second_clock::local_time();
  boost::posix_time::time_duration td = ts - getTimestamp();
  return (td.total_seconds() > timeoutSeconds);
}

void SignalState::addChangeListener(StateListenerPtr listener) {
  listeners.push_back(listener);
}

void SignalState::fireStateChange(SignalStatePtr before, SignalStatePtr after) {
  StateEventPtr event = boost::make_shared<StateEvent>(before, after);
  for(auto& listener : listeners) {
    listener->stateChanged(event);
  }
}

void SignalState::updateTimestamp() {
  timestamp = boost::posix_time::second_clock::local_time();
}

const boost::posix_time::ptime& SignalState::getTimestamp() const {
  return timestamp;
}

CallProtocol::PayloadPtr AnalogState::create() {
	return boost::make_shared<AnalogState>();
}

AnalogState::AnalogState()
  : SignalState(
      AI,
      TIMEOUT_SECONDS,
      EXPIRE_SECONDS,
      boost::posix_time::ptime()),
    value() {

}

AnalogState::AnalogState(const AnalogState& r)
  : SignalState(r),
    value(r.value) {

}

AnalogState::~AnalogState() {

}

AnalogState& AnalogState::operator=(const AnalogState& r) {
  SignalState::operator=(r);

  this->value = r.value;

  return *this;
}

void AnalogState::setValue(double v) {
  SignalStatePtr before = clone();
  value =  v;
  updateTimestamp();	
  SignalStatePtr after =  clone();

  fireStateChange(before, after);
}

double AnalogState::getValue() const {
  return value;
}

void AnalogState::load(boost::asio::streambuf& sb) {
  SignalState::load(sb);
  boost::endian::big_uint64_buf_t repr;
  sb.sgetn(reinterpret_cast<char*>(&repr), sizeof(repr));
  value = *(reinterpret_cast<double*>(&repr));
}

void AnalogState::store(boost::asio::streambuf& sb) const {
  SignalState::store(sb);
  boost::endian::big_uint64_buf_t repr;
  memcpy(&repr, &value, sizeof(repr));
  sb.sputn(repr.data(), sizeof(repr));
}

std::size_t AnalogState::size() const {
  return SignalState::size() + sizeof(value);
}

SignalStatePtr AnalogState::clone() const {
  SignalStatePtr ptr(new AnalogState(*this));
  return ptr;
}

CallProtocol::PayloadPtr BooleanState::create() {
	return boost::make_shared<BooleanState>();
}

BooleanState::BooleanState()
  : SignalState(
      DI,
      TIMEOUT_SECONDS,
      EXPIRE_SECONDS,
      boost::posix_time::ptime()),
    value() {

}

BooleanState::BooleanState(const BooleanState& r)
  : SignalState(r),
    value(r.value) {

}

BooleanState::~BooleanState() {

}

BooleanState& BooleanState::operator=(const BooleanState& r) {
  SignalState::operator=(r);

  this->value = r.value;

  return *this;
}

void BooleanState::setValue(bool v) {
  SignalStatePtr before = clone();
  value =  v;
  SignalStatePtr after =  clone();

  fireStateChange(before, after);
}

bool BooleanState::getValue() const {
  return value;
}

void BooleanState::load(boost::asio::streambuf& sb) {
  SignalState::load(sb);
  boost::endian::little_uint8_buf_t repr;
  sb.sgetn(reinterpret_cast<char*>(&repr), sizeof(repr));
  value = *reinterpret_cast<bool*>(&repr);
}

void BooleanState::store(boost::asio::streambuf& sb) const {
  SignalState::store(sb);
  boost::endian::little_uint8_buf_t repr;
  repr = value;
  sb.sputn(repr.data(), sizeof(repr));
}

std::size_t BooleanState::size() const {
  return SignalState::size() + sizeof(value);
}

SignalStatePtr BooleanState::clone() const {
  SignalStatePtr ptr(new BooleanState(*this));
  return ptr;
}

CallProtocol::PayloadPtr StringState::create() {
	return boost::make_shared<StringState>();
}

StringState::StringState()
  : SignalState(
      SI,
      TIMEOUT_SECONDS,
      EXPIRE_SECONDS,
      boost::posix_time::ptime()),
    value() {

}

StringState::StringState(const StringState& r)
  : SignalState(r),
    value(r.value) {

}

StringState::~StringState() {

}

StringState& StringState::operator=(const StringState& r) {
  SignalState::operator=(r);

  this->value = r.value;

  return *this;
}

void StringState::setValue(const string& v) {
  SignalStatePtr before = clone();
  value =  v;
  updateTimestamp();
  SignalStatePtr after =  clone();

  fireStateChange(before, after);
}

string StringState::getValue() const {
  return value;
}

void StringState::load(boost::asio::streambuf& sb) {
  SignalState::load(sb);
  uint32_t len;
  boost::endian::little_uint32_buf_t lenRepr;
  sb.sgetn(reinterpret_cast<char*>(&lenRepr), sizeof(lenRepr));
  len = *reinterpret_cast<uint32_t*>(&lenRepr);
  for(size_t i = 0; i != len; ++i) {
    char c = sb.sbumpc();
    value.push_back(c);
  }
}

void StringState::store(boost::asio::streambuf& sb) const {
  SignalState::store(sb);
  uint32_t len = value.length();
  boost::endian::little_uint32_buf_t lenRepr;
  lenRepr = len;
  sb.sputn(lenRepr.data(), sizeof(lenRepr));
  for(auto& c : value) {
    sb.sputc(c);
  }
}

std::size_t StringState::size() const {
  return SignalState::size() + value.length();
}

SignalStatePtr StringState::clone() const {
  SignalStatePtr ptr(new StringState(*this));
  return ptr;
}

PayloadPtr GetStateRequest::create() {
	return boost::make_shared<GetStateRequest>();
}

GetStateRequest::GetStateRequest() :signalIds() {

}

GetStateRequest::GetStateRequest(const std::list<SignalId>& l)
:signalIds(l) {

}

GetStateRequest::GetStateRequest(const GetStateRequest& r)
:signalIds(r.signalIds) {

}

GetStateRequest::~GetStateRequest() {

}

GetStateRequest& GetStateRequest::operator=(const GetStateRequest& r) {
  this->signalIds = r.signalIds;
  return *this;
}

const GetStateRequest::SignalIds& GetStateRequest::getSignalIds() const {
  return signalIds;
};

void GetStateRequest::addSignalId(const SignalId& s) {
  signalIds.push_back(s);
}

void GetStateRequest::load(boost::asio::streambuf& sb) {
  uint32_t len;
  boost::endian::little_uint32_buf_t lenRepr;
  sb.sgetn(reinterpret_cast<char*>(&lenRepr), sizeof(lenRepr));
  len = *reinterpret_cast<uint32_t*>(&lenRepr);
  for(size_t i = 0; i != len; ++i) {
    SignalId id;
    id.load(sb);
    signalIds.push_back(id);
  }
}

void GetStateRequest::store(boost::asio::streambuf& sb) const {
  uint32_t len = signalIds.size();
  boost::endian::little_uint32_buf_t lenRepr;
  lenRepr = len;
  sb.sputn(lenRepr.data(), sizeof(lenRepr));
  for(auto& c : signalIds) {
    c.store(sb);
  }
}

std::size_t GetStateRequest::size() const {
  std::size_t length = sizeof(uint32_t);
  for(auto& c : signalIds) {
    length += c.size();
  }
  return length;
}

PayloadPtr GetStateResponse::create() {
  return boost::make_shared<GetStateResponse>();
}

GetStateResponse::GetStateResponse() :signals() {

}

GetStateResponse::GetStateResponse(const Signals& l)
:signals(l) {

}

GetStateResponse::GetStateResponse(const GetStateResponse& r)
:signals(r.signals) {

}

GetStateResponse::~GetStateResponse() {

}

GetStateResponse& GetStateResponse::operator=(const GetStateResponse& r) {
  this->signals = r.signals;
  return *this;
}

void GetStateResponse::addSignal(const SignalId& id, SignalStatePtr s) {
  signals.insert(std::make_pair(id, s));
}

const SignalStatePtr GetStateResponse::getSignal(const SignalId& id) const {
  const auto& it = signals.find(id);
  if(it != signals.end()) {
    return it->second;
  }
  return SignalStatePtr();
}

void GetStateResponse::load(boost::asio::streambuf& sb) {
  uint32_t len;
  boost::endian::little_uint32_buf_t lenRepr;
  sb.sgetn(reinterpret_cast<char*>(&lenRepr), sizeof(lenRepr));
  len = *reinterpret_cast<uint32_t*>(&lenRepr);
  for(size_t i = 0; i != len; ++i) {
    SignalId id;
    id.load(sb);
    SignalStatePtr s = SignalState::createFrom(sb);
    signals.insert(std::make_pair(id, s));
  }
}

void GetStateResponse::store(boost::asio::streambuf& sb) const {
  uint32_t len = signals.size();
  boost::endian::little_uint32_buf_t lenRepr;
  lenRepr = len;
  sb.sputn(lenRepr.data(), sizeof(lenRepr));
  for(auto& c : signals) {
    c.first.store(sb);
    c.second->store(sb);
  }
}

std::size_t GetStateResponse::size() const {
  std::size_t length = sizeof(uint32_t);
  for(auto& c : signals) {
    length += c.first.size();
    length += c.second->size();
  }
  return length;
}

PayloadPtr SetStateRequest::create() {
  return boost::make_shared<SetStateRequest>();
}

SetStateRequest::SetStateRequest() :signals() {

}

SetStateRequest::SetStateRequest(const Signals& l)
:signals(l) {

}

SetStateRequest::SetStateRequest(const SetStateRequest& r)
:signals(r.signals) {

}

SetStateRequest::~SetStateRequest() {

}

SetStateRequest& SetStateRequest::operator=(const SetStateRequest& r) {
  this->signals = r.signals;
  return *this;
}

void SetStateRequest::addSignal(const SignalId& id, SignalStatePtr s) {
  signals.insert(std::make_pair(id, s));
}

void SetStateRequest::load(boost::asio::streambuf& sb) {
  uint32_t len;
  boost::endian::little_uint32_buf_t lenRepr;
  sb.sgetn(reinterpret_cast<char*>(&lenRepr), sizeof(lenRepr));
  len = *reinterpret_cast<uint32_t*>(&lenRepr);
  for(size_t i = 0; i != len; ++i) {
    SignalId id;
    id.load(sb);
    SignalStatePtr s = SignalState::createFrom(sb);
    signals.insert(std::make_pair(id, s));
  }
}

void SetStateRequest::store(boost::asio::streambuf& sb) const {
  uint32_t len = signals.size();
  boost::endian::little_uint32_buf_t lenRepr;
  lenRepr = len;
  sb.sputn(lenRepr.data(), sizeof(lenRepr));
  for(auto& c : signals) {
    c.first.store(sb);
    c.second->store(sb);
  }
}

std::size_t SetStateRequest::size() const {
  std::size_t length = sizeof(uint32_t);
  for(auto& c : signals) {
    length += c.first.size();
    length += c.second->size();
  }
  return length;
}

PayloadPtr SetStateResponse::create() {
	return boost::make_shared<SetStateResponse>();
}

SetStateResponse::SetStateResponse() :results() {

}

SetStateResponse::SetStateResponse(const Results& l)
:results(l) {

}

SetStateResponse::SetStateResponse(const SetStateResponse& r)
:results(r.results) {

}

SetStateResponse::~SetStateResponse() {

}

SetStateResponse& SetStateResponse::operator=(const SetStateResponse& r) {
  this->results = r.results;
  return *this;
}

void SetStateResponse::addResult(const SignalId& id, Result s) {
  results.insert(std::make_pair(id, s));
}

void SetStateResponse::load(boost::asio::streambuf& sb) {
  uint32_t len;
  boost::endian::little_uint32_buf_t lenRepr;
  sb.sgetn(reinterpret_cast<char*>(&lenRepr), sizeof(lenRepr));
  len = *reinterpret_cast<uint32_t*>(&lenRepr);
  for(size_t i = 0; i != len; ++i) {
    SignalId id;
    id.load(sb);
    uint8_t s = sb.sbumpc();
    results.insert(std::make_pair(id, s));
  }
}

void SetStateResponse::store(boost::asio::streambuf& sb) const {
  uint32_t len = results.size();
  boost::endian::little_uint32_buf_t lenRepr;
  lenRepr = len;
  sb.sputn(lenRepr.data(), sizeof(lenRepr));
  for(auto& c : results) {
    c.first.store(sb);
    sb.sputc(c.second);
  }
}

std::size_t SetStateResponse::size() const {
  std::size_t length = sizeof(uint32_t);
  for(auto& c : results) {
    length += c.first.size();
    length += sizeof(c.second);
  }
  return length;
}

void PayloadFactoryInitializer(CallProtocol::PayloadFactory& factory) {
  factory.addCreator(SignalId::TYPE_ID, boost::bind(&SignalId::create));
  factory.addCreator(StateEvent::TYPE_ID, boost::bind(&StateEvent::create));
  factory.addCreator(AnalogState::TYPE_ID, boost::bind(&AnalogState::create));
  factory.addCreator(BooleanState::TYPE_ID, boost::bind(&BooleanState::create));
  factory.addCreator(StringState::TYPE_ID, boost::bind(&StringState::create));
  factory.addCreator(GetStateRequest::TYPE_ID, boost::bind(&GetStateRequest::create));
  factory.addCreator(GetStateResponse::TYPE_ID, boost::bind(&GetStateResponse::create));
  factory.addCreator(SetStateRequest::TYPE_ID, boost::bind(&SetStateRequest::create));
  factory.addCreator(SetStateResponse::TYPE_ID, boost::bind(&SetStateResponse::create));
}

} /* namespace DataObjects */
