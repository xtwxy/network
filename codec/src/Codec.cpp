/*
 * Codec.cpp
 *
 *  Created on: Apr 22, 2016
 *      Author: master
 */

#include <boost/bind.hpp>
#include "Codec.h"

namespace codec {

PipelineImpl::PipelineImpl(SessionPtr ssn, const std::size_t bufferSize) :
    session(ssn),
    BUFFER_SIZE(bufferSize),
    readBuffer(new char[BUFFER_SIZE]),
    writeBuffer(new char[BUFFER_SIZE]),
    bytesRead(0),
    bytesToRead(0),
    bytesWritten(0),
    bytesToWrite(0) {
    }

PipelineImpl::~PipelineImpl() {
  delete[] readBuffer;
  delete[] writeBuffer;
}

void PipelineImpl::addLast(EncoderPtr encoder) {
  ContextPtr ctx(new Context(*this));
  EncoderContext encoderContext(ctx, encoder);
  encoderContexts.push_back(encoderContext);
}

void PipelineImpl::addLast(DecoderPtr decoder) {
  ContextPtr ctx(new Context(*this));
  DecoderContext decoderContext(ctx, decoder);
  decoderContexts.push_back(decoderContext);
}

void PipelineImpl::remove(EncoderPtr encoder) {
  encoderContexts.erase(
      std::remove_if(encoderContexts.begin(), encoderContexts.end(),
                     [&encoder] (EncoderContext& encoderContext) {
                     return (encoderContext.get<1>() == encoder);
                     })
      );
}

void PipelineImpl::remove(DecoderPtr decoder) {
  decoderContexts.erase(
      std::remove_if(decoderContexts.begin(), decoderContexts.end(),
                     [&decoder] (DecoderContext& decoderContext) {
                     return (decoderContext.get<1>() == decoder);
                     })
      );
}

void PipelineImpl::setHandler(HandlerPtr handler) {
  this->handler = handler;
}

void PipelineImpl::close() {
  session->close();
}

void PipelineImpl::read() {
	// 2.start reading.
	session->read((readBuffer + bytesRead), (BUFFER_SIZE - bytesRead),
			boost::bind(&PipelineImpl::onReadComplete, shared_from_this(), _1,
					_2));
}

void PipelineImpl::write() {
	// 2.start reading.
	session->read((readBuffer + bytesWritten), (BUFFER_SIZE - bytesToWrite),
			boost::bind(&PipelineImpl::onWriteComplete, shared_from_this(), _1,
					_2));
}

void PipelineImpl::start() {
  // 1.notify encoders/decoders that session has commenced.
	processSessionStart();
  // 2.start reading.
	read();
  // 3.start event dispatching.
}

void PipelineImpl::onReadComplete(
		const boost::system::error_code& ec,
		std::size_t bytesTransfered) {
	if(!ec) {
		bytesRead += bytesTransfered;
		processDataArrive();
		read();
	} else {
		processExceptionCaught(ec);
	}
}

void PipelineImpl::onWriteComplete(
		const boost::system::error_code& ec,
		std::size_t bytesTransfered) {
	if(!ec) {
		bytesWritten += bytesTransfered;
		if(bytesWritten == bytesToWrite) {
			if(!WriteRequestQueue.empty()) {
				WriteRequestQueue.front()->onComplete();
				WriteRequestQueue.pop();
				auto cmd = WriteRequestQueue.front();
				cmd();
			}
		}
	} else {
		processExceptionCaught(ec);
	}
}
void PipelineImpl::onTimeout(const boost::system::error_code& ec) {
	if(!ec) {
		processTimeout();
	} else {
		processExceptionCaught(ec);
	}
}

void PipelineImpl::processSessionStart() {
  std::for_each(decoderContexts.begin(), decoderContexts.end(),
                [] (DecoderContext& decoderContext) {
                decoderContext.get<1>()->sessionStart(decoderContext.get<1>());
                });
  std::for_each(encoderContexts.begin(), encoderContexts.end(),
                [] (EncoderContext& encoderContext) {
                encoderContext.get<1>()->sessionStart(encoderContext.get<1>());
                });
}

void PipelineImpl::processSessionClose() {
	  std::for_each(decoderContexts.begin(), decoderContexts.end(),
	                [] (DecoderContext& decoderContext) {
	                decoderContext.get<1>()->sessionClose(decoderContext.get<1>());
	                });
	  std::for_each(encoderContexts.begin(), encoderContexts.end(),
	                [] (EncoderContext& encoderContext) {
	                encoderContext.get<1>()->sessionClose(encoderContext.get<1>());
	                });
}

void PipelineImpl::processExceptionCaught(const boost::system::error_code& ec) {
	//TODO: add error code to exception transformation.
	std::exception ex;
	  std::for_each(decoderContexts.begin(), decoderContexts.end(),
	                [] (DecoderContext& decoderContext) {
	                decoderContext.get<1>()->exceptionCaught(decoderContext.get<1>(), ex);
	                });
	  std::for_each(encoderContexts.begin(), encoderContexts.end(),
	                [] (EncoderContext& encoderContext) {
	                encoderContext.get<1>()->exceptionCaught(encoderContext.get<1>(), ex);
	                });
}

void PipelineImpl::processDataArrive() {

}

void PipelineImpl::processWriteComplete() {
}

void PipelineImpl::processTimeout() {
	//TODO: add timeout exception transformation.
	std::exception ex;
	  std::for_each(decoderContexts.begin(), decoderContexts.end(),
	                [] (DecoderContext& decoderContext) {
	                decoderContext.get<1>()->exceptionCaught(decoderContext.get<1>(), ex);
	                });
	  std::for_each(encoderContexts.begin(), encoderContexts.end(),
	                [] (EncoderContext& encoderContext) {
	                encoderContext.get<1>()->exceptionCaught(encoderContext.get<1>(), ex);
	                });
}


Pipeline::Pipeline(SessionPtr ssn, const std::size_t bufferSize) :
		impl(ssn, bufferSize) {
    }

PipelineImpl::~PipelineImpl() {
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

Encoder::Encoder(const EncodeFunc f, const SessionStart s, const SessionClose c,
                 const ExceptionCaught e) :
    encode(f), sessionStart(s), sessionClose(c), exceptionCaught(e) {

    }

Encoder::~Encoder() {

}

Decoder::Decoder(const EncodeFunc f, const SessionStart s, const SessionClose c,
                 const ExceptionCaught e) :
    decode(f), sessionStart(s), sessionClose(c), exceptionCaught(e) {

    }

Decoder::~Decoder() {

}

Handler::Handler(const HandlerFunc f, const SessionStart s, const SessionClose c,
                 const ExceptionCaught e) :
    handle(f), sessionStart(s), sessionClose(c), exceptionCaught(e) {

    }

Handler::~Handler() {

}

Session::Session(Read r, Write w, Post p, Close c) :
    read(r), write(w), post(p), close(c) {

    }

Session::~Session() {

}

} /* namespace codec */
