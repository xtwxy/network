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

Message* CodecMessageFactory::createMessage(const MessageType type) {
	auto pos = delegates.find(type);
	if(pos != delegates.end()) {
		return pos->second->createMessage(type);
	}
	return nullptr;
}

void CodecMessageFactory::deleteMessage(const Message* message) {
	auto pos = delegates.find(message->getTypeId());
	if(pos != delegates.end()) {
		pos->second->deleteMessage(message);
	}
}

void CodecMessageFactory::setMessageTypeId(const MessageType) {
	assert(false);
}

void CodecMessageFactory::add(MessageFactory* f) {
	delegates.insert(std::make_pair(typeCount++, f));
}

MessageType CodecMessageFactory::typeCount = 0;
CodecMessageFactory CodecMessageFactory::instance;

CodecMessageFactory& CodecMessageFactory::getInstance() {
	return CodecMessageFactory::instance;
}


} // namespace CallProtocol

