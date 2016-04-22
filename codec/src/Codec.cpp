/*
 * Codec.cpp
 *
 *  Created on: Apr 22, 2016
 *      Author: master
 */

#include "Codec.h"

namespace codec {

Pipeline::Pipeline(SessionPtr ssn) : session(ssn) {

}

Pipeline::~Pipeline() {

}

void Pipeline::addLast(EncoderPtr encoder) {
  encoders.push_back(encoder);
}

void Pipeline::addLast(DecoderPtr decoder) {
  decoders.push_back(decoder);
}

void Pipeline::remove(EncoderPtr encoder) {
  encoders.remove(encoder);
}

void Pipeline::remove(DecoderPtr decoder) {
  decoders.remove(decoder);
}

void Pipeline::setHandler(HandlerPtr handler) {
  this->handler = handler;
}

void Pipeline::close() {
  session->close();
}

void Pipeline::start() {

}

Context::Context(Pipeline& p) : pipeline(p) {

}

Context::~Context() {

}

Pipeline& Context::getPipeline() {
  return pipeline;
}

void Context::write(boost::any&) {

}

void Context::close() {
  pipeline.close();
}

Encoder::Encoder(const EncodeFunc f, const SessionClose c,
		const ExceptionCaught e) :
		encode(f), sessionClose(c), exceptionCaught(e) {

}

Encoder::~Encoder() {

}

Decoder::Decoder(const DecodeFunc f, const SessionClose c,
		const ExceptionCaught e) :
		decode(f), sessionClose(c), exceptionCaught(e) {

}

Decoder::~Decoder() {

}

Handler::Handler(const HandlerFunc f, const SessionClose c,
		const ExceptionCaught e) :
				handle(f), sessionClose(c), exceptionCaught(e) {

}

Handler::~Handler() {

}

Session::Session(Read r, Write w, Post p, Close c) :
    read(r), write(w), post(p), close(c) {

    }

Session::~Session() {

}

} /* namespace codec */
