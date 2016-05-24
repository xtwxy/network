#include "CallProtocol.h"

namespace CallProtocol {

uint16_t MessageHeader::getLength() {
	return length.value();
}

void MessageHeader::setLength(uint16_t l) {
	length = l;
}

MessageType MessageHeader::getType() {
	return type.value();
}

void MessageHeader::setType(const MessageType t) {
	type = t;
}

uint16_t Message::getLength() {
	return header.getLength();
}

void Message::setLength(uint16_t l) {
	header.setLength(l);
}

MessageType Message::getType() {
	return header.getType();
}
void Message::setType(const MessageType t) {
	header.setType(t);
}

Correlation TwowayMessage::getCorrelation() {
	return correlation.value();
}

void TwowayMessage::setCorrelation(Correlation c) {
	correlation = c;
}


} // namespace CallProtocol

