#include "CallProtocol.h"

namespace CallProtocol {

uint16_t MessageHeader::getLength() const {
	return length.value();
}

void MessageHeader::setLength(uint16_t l) {
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

MessageHandlerFactory::MessageHandlerFactory() {

}

MessageHandlerFactory::~MessageHandlerFactory() {

}

codec::HandlerPtr MessageHandlerFactory::getHandler(
		const MessageType type) const {
	auto pos = messageHandlers.find(type);
	if (pos != messageHandlers.end()) {
		return pos->second;
	}
	throw std::invalid_argument("Invalid message type: " + type);
}

void MessageHandlerFactory::addHandler(const MessageType type,
		codec::HandlerPtr h) {
	messageHandlers.insert(std::make_pair(type, h));
}

} // namespace CallProtocol

