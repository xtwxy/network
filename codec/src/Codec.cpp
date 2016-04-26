/*
 * Codec.cpp
 *
 *  Created on: Apr 22, 2016
 *      Author: master
 */

#include <boost/bind.hpp>
#include "Codec.h"

namespace codec {

PipelineImpl::PipelineImpl(Pipeline& p,
		SessionPtr ssn, const std::size_t bufferSize) :
	parent(p),
    session(ssn),
    BUFFER_SIZE(bufferSize),
    readBuffer() {
    }

PipelineImpl::~PipelineImpl() {
}

void PipelineImpl::addLast(EncoderPtr encoder) {
  ContextPtr ctx(new Context(parent));
  EncoderContext encoderContext(ctx, encoder);
  encoderContexts.push_back(encoderContext);
}

void PipelineImpl::addLast(DecoderPtr decoder) {
  ContextPtr ctx(new Context(parent));
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
  ContextPtr ctx(new Context(parent));
  this->handlerContext = HandlerContext(ctx, handler);
}

void PipelineImpl::close() {
  session->close();
}

void PipelineImpl::read() {
	// 2.start reading.
	session->read(readBuffer.prepare(BUFFER_SIZE),
			boost::bind(&PipelineImpl::onReadComplete, shared_from_this(), _1,
					_2));
}

void PipelineImpl::write() {
	// 2.start writing.
	if (!writeRequestQueue.empty()) {
		boost::any any = writeRequestQueue.front()->data;
		if(any.type() == typeid(boost::asio::streambuf*)) {
			boost::asio::streambuf* buffer =
					boost::any_cast<boost::asio::streambuf*>(any);
			session->write(buffer->data(),
					boost::bind(&PipelineImpl::onWriteComplete, shared_from_this(), _1,
							_2));
		} else {
			// invalid data!
		}
	} else {
		// nothing to write!
	}

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
		readBuffer.commit(bytesTransfered);
		processDataArrive();
		read();
	} else {
		processExceptionCaught(ec);
	}
}

void PipelineImpl::onWriteComplete(
		const boost::system::error_code& ec,
		std::size_t bytesTransfered) {
	if (!ec) {
		if (!writeRequestQueue.empty()) {
			boost::any any = writeRequestQueue.front()->data;
			if(any.type() == typeid(boost::asio::streambuf*)) {
				boost::asio::streambuf* buffer =
						boost::any_cast<boost::asio::streambuf*>(any);
				buffer->consume(bytesTransfered);
				writeRequestQueue.front()->action();
			} else {
				// invalid data!
			}
			writeRequestQueue.pop();
		} else {
			// nothing to write!
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
                decoderContext.get<1>()->sessionStart(*decoderContext.get<0>());
                });
  std::for_each(encoderContexts.begin(), encoderContexts.end(),
                [] (EncoderContext& encoderContext) {
                encoderContext.get<1>()->sessionStart(*encoderContext.get<0>());
                });
}

void PipelineImpl::processSessionClose() {
	  std::for_each(decoderContexts.begin(), decoderContexts.end(),
	                [] (DecoderContext& decoderContext) {
	                decoderContext.get<1>()->sessionClose(*decoderContext.get<0>());
	                });
	  std::for_each(encoderContexts.begin(), encoderContexts.end(),
	                [] (EncoderContext& encoderContext) {
	                encoderContext.get<1>()->sessionClose(*encoderContext.get<0>());
	                });
}

void PipelineImpl::processExceptionCaught(const boost::system::error_code& ec) {
	//TODO: add error code to exception transformation.
	std::exception ex;
	  std::for_each(decoderContexts.begin(), decoderContexts.end(),
	                [ex] (DecoderContext& decoderContext) {
	                decoderContext.get<1>()->exceptionCaught(*decoderContext.get<0>(), ex);
	                });
	  std::for_each(encoderContexts.begin(), encoderContexts.end(),
	                [ex] (EncoderContext& encoderContext) {
	                encoderContext.get<1>()->exceptionCaught(*encoderContext.get<0>(), ex);
	                });
}

void PipelineImpl::processDataArrive() {
	std::list<boost::any> in;

	in.push_back(&readBuffer);

	std::list<boost::any> out;

	// call decoders.
	for(auto dc = decoderContexts.begin(); dc != decoderContexts.end(); ++dc) {
		for(auto it = in.begin(); it != in.end(); ++it) {
			dc->get<1>()->decode(*dc->get<0>(),
					*it,
					out);
		}
		in = out;
	}
	// call handlers.
	for(auto it = out.begin(); it != out.end(); ++it) {
		handlerContext.get<1>()->handle(*handlerContext.get<0>(), *it);
	}

	// check possible write backs.
	in = handlerContext.get<0>()->getOut();
	// call decoders.
	for(auto dc = encoderContexts.begin(); dc != encoderContexts.end(); ++dc) {

		in.insert(dc->get<0>()->getOut().begin(),
				dc->get<0>()->getOut().end());

		for(auto it = in.begin(); it != in.end(); ++it) {
			dc->get<1>()->encode(*dc->get<0>(),
					*it,
					out);
		}
		in = out;
	}

}

void PipelineImpl::processTimeout() {
	TimeoutException ex;
	  std::for_each(decoderContexts.begin(), decoderContexts.end(),
	                [ex] (DecoderContext& decoderContext) {
	                decoderContext.get<1>()->exceptionCaught(*decoderContext.get<0>(), ex);
	                });
	  std::for_each(encoderContexts.begin(), encoderContexts.end(),
	                [ex] (EncoderContext& encoderContext) {
	                encoderContext.get<1>()->exceptionCaught(*encoderContext.get<0>(), ex);
	                });
}


Pipeline::Pipeline(SessionPtr ssn, const std::size_t bufferSize) :
		impl(*this, ssn, bufferSize) {
    }

Pipeline::~Pipeline() {
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
