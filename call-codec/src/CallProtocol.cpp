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

uint16_t Message::getLength() const {
	return header.getLength();
}

void Message::setLength(uint16_t l) {
	header.setLength(l);
}

MessageType Message::getTypeId() const {
	return header.getTypeId();
}

void Message::setTypeId(const MessageType t) {
	header.setTypeId(t);
}

MessageFactory::MessageFactory() {

}

MessageFactory::~MessageFactory() {

}

CodecMessageFactory::CodecMessageFactory() {

}

CodecMessageFactory::~CodecMessageFactory() {

}

MessagePtr CodecMessageFactory::createMessage(const MessageType type) const {
	auto pos = delegates.find(type);
	if(pos != delegates.end()) {
		return pos->second->createMessage();
	}
	return nullptr;
}

void CodecMessageFactory::add(const MessageType type, MessageFactoryPtr f) {
	delegates.insert(std::make_pair(type, f));
}

} // namespace CallProtocol

