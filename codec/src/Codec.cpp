/*
 * Codec.cpp
 *
 *  Created on: Apr 22, 2016
 *      Author: master
 */

#include <boost/bind.hpp>
#include "Codec.h"

namespace codec {

TimeoutException::TimeoutException() {

}

TimeoutException::~TimeoutException() {

}

WriteRequest::WriteRequest(boost::any& data)
: data(data), action([](){}){

}

WriteRequest::WriteRequest(boost::any& data, CompletionHandler a) : data(data), action(a) {

}

WriteRequest::WriteRequest(boost::any& data, WriteRequest::Ptr upperStreamReq)
  : data(data), action(upperStreamReq->action) {

}

WriteRequest::~WriteRequest() {

}

void WriteRequest::onComplete() {
	if(action) {
		action();
	}
}

boost::any& WriteRequest::getData() {
	return data;
}

PipelineImpl::PipelineImpl(Pipeline& p,
		SessionPtr ssn, const std::size_t bufferSize) :
	parent(p),
    session(ssn),
    BUFFER_SIZE(bufferSize),
    readBuffer() {
    }

PipelineImpl::~PipelineImpl() {
}

void PipelineImpl::addLast(CodecPtr codec) {
  ContextPtr ctx(new Context(parent));
  CodecContext codecContext(ctx, codec);
  codecContexts.push_back(codecContext);
}

void PipelineImpl::remove(CodecPtr codec) {
	codecContexts.erase(
      std::remove_if(codecContexts.begin(), codecContexts.end(),
                     [&codec] (CodecContext& codecContext) {
                     return (codecContext.get<1>() == codec);
                     })
      );
}

void PipelineImpl::setHandler(HandlerPtr handler) {
  ContextPtr ctx(new Context(parent));
  this->handlerContext = HandlerContext(ctx, handler);
}

void PipelineImpl::write(boost::any& output) {
	std::list<boost::any> in;

	in.push_back(output);

	std::list<boost::any> out;

	for(auto dc = codecContexts.begin(); dc != codecContexts.end(); ++dc) {
		for(auto it = in.begin(); it != in.end(); ++it) {
			dc->get<1>()->encode(*dc->get<0>(),
					*it,
					out);
		}
		in = out;
	}
	enqueueWriteRequest(in);
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

void PipelineImpl::dequeueWriteRequest() {
	// 2.start writing.
	if (!writeRequestQueue.empty()) {
		boost::any& any = writeRequestQueue.front()->getData();
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

void PipelineImpl::enqueueWriteRequest(std::list<boost::any>& out) {
	boost::mutex::scoped_lock lock(mutex);
	for(auto it = out.begin(); it != out.end(); ++it) {
		WriteRequest::Ptr request(new WriteRequest(*it));
		writeRequestQueue.push(request);
	}
}

void PipelineImpl::enqueueWriteRequest(std::list<boost::any>& out, CompletionHandler) {
	assert(false);
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
			boost::any& any = writeRequestQueue.front()->getData();
			if(any.type() == typeid(boost::asio::streambuf*)) {
				boost::asio::streambuf* buffer =
						boost::any_cast<boost::asio::streambuf*>(any);
				buffer->consume(bytesTransfered);
				writeRequestQueue.front()->onComplete();
			} else {
				// invalid data!
			}
			writeRequestQueue.pop();
			dequeueWriteRequest();
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
  std::for_each(codecContexts.begin(), codecContexts.end(),
                [] (CodecContext& codecContext) {
                codecContext.get<1>()->sessionStart(*codecContext.get<0>());
                });
}

void PipelineImpl::processSessionClose() {
	  std::for_each(codecContexts.begin(), codecContexts.end(),
	                [] (CodecContext& codecContext) {
	                codecContext.get<1>()->sessionClose(*codecContext.get<0>());
	                });
}

void PipelineImpl::processExceptionCaught(const boost::system::error_code& ec) {
	//TODO: add error code to exception transformation.
	std::exception ex;
	  std::for_each(codecContexts.begin(), codecContexts.end(),
	                [ex] (CodecContext& codecContext) {
	                codecContext.get<1>()->exceptionCaught(*codecContext.get<0>(), ex);
	                });
}

void PipelineImpl::processDataArrive() {
	std::list<boost::any> in;

	in.push_back(&readBuffer);

	std::list<boost::any> out;

	// call decoders.
	for(auto dc = codecContexts.begin(); dc != codecContexts.end(); ++dc) {
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
	in = handlerContext.get<0>()->getOutputs();
	// call decoders.
	for(auto dc = codecContexts.begin(); dc != codecContexts.end(); ++dc) {

		in.insert(dc->get<0>()->getOutputs().begin(),
				dc->get<0>()->getOutputs().end());

		for(auto it = in.begin(); it != in.end(); ++it) {
			dc->get<1>()->encode(*dc->get<0>(),
					*it,
					out);
		}
		in = out;
	}
	enqueueWriteRequest(out);
}

void PipelineImpl::processTimeout() {
	TimeoutException ex;
	  std::for_each(codecContexts.begin(), codecContexts.end(),
	                [ex] (CodecContext& codecContext) {
	                codecContext.get<1>()->exceptionCaught(*codecContext.get<0>(), ex);
	                });
}


Pipeline::Pipeline(SessionPtr ssn, const std::size_t bufferSize)
: impl(*this, ssn, bufferSize) {
}

Pipeline::~Pipeline() {
}

void Pipeline::addLast(CodecPtr codec) {
	impl.addLast(codec);
}

void Pipeline::remove(CodecPtr codec) {
	impl.remove(codec);
}

void Pipeline::setHandler(HandlerPtr handler) {
	impl.setHandler(handler);
}

void Pipeline::write(boost::any& out) {
	impl.write(out);
}

void Pipeline::close() {
	impl.close();
}


Context::Context(Pipeline& p) : pipeline(p) {

}

Context::~Context() {

}

Pipeline& Context::getPipeline() {
  return pipeline;
}

void Context::write(boost::any& output) {
	outputs.push_back(output);
}

void Context::close() {
  pipeline.close();
}

std::list<boost::any>& Context::getOutputs() {
	return outputs;
}

Codec::Codec(const EncodeFunc ef, DecodeFunc df, const SessionStart s, const SessionClose c,
                 const ExceptionCaught e) :
    encode(ef), decode(df), sessionStart(s), sessionClose(c), exceptionCaught(e) {

    }

Codec::~Codec() {

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
