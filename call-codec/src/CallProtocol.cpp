#include "CallProtocol.h"

namespace CallProtocol {

MessageHeader::MessageHeader() {

}

MessageHeader::MessageHeader(uint32_t len,
			MessageType type,
			Correlation c)
	: length(len),
	  typeId(type),
	  correlation(c) {

}
uint32_t MessageHeader::getLength() const {
	return length.value();
}

void MessageHeader::setLength(uint32_t l) {
	length = l;
}

MessageType MessageHeader::getTypeId() const {
	return typeId.value();
}

void MessageHeader::setTypeId(const MessageType t) {
	typeId = t;
}

Correlation MessageHeader::getCorrelation() const {
	return correlation.value();
}

void MessageHeader::setCorrelation(Correlation c) {
	correlation = c;
}

Payload::Payload () {

}

Payload::~Payload() {

}

boost::any Payload::any() {
	return boost::any(shared_from_this());
}

PayloadFactory::PayloadFactory(PayloadFactoryInitializer initialize) {
	initialize(*this);
}
PayloadFactory::PayloadFactory() {

}
PayloadFactory::~PayloadFactory() {

}

void PayloadFactory::addCreator(const MessageType t, PayloadCreator c) {
	payloadCreators.insert(std::make_pair(t, c));
}
PayloadPtr PayloadFactory::createPayload(const MessageType t) const {
	auto it = payloadCreators.find(t);
	if(it != payloadCreators.end()) {
		return it->second(t);
	}
	throw std::invalid_argument("Invalid message type: ");
}

Message::Message() {

}

Message::Message(uint32_t len,
			MessageType type,
			Correlation c,
			Payload::Ptr payload)
	: header(len, type, c), payload(payload) {

}

uint32_t Message::getLength() const {
	return header.getLength();
}

void Message::setLength(uint32_t l) {
	header.setLength(l);
}

MessageType Message::getTypeId() const {
	return header.getTypeId();
}

void Message::setTypeId(const MessageType t) {
	header.setTypeId(t);
}

Correlation Message::getCorrelation() const {
	return header.getCorrelation();
}

void Message::setCorrelation(Correlation c) {
	header.setCorrelation(c);
}

} // namespace CallProtocol

