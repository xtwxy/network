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
: data(data), action([]() {}) {

}

WriteRequest::WriteRequest(boost::any& data, CompletionHandler a) :
    data(data), action(a) {

}

WriteRequest::WriteRequest(boost::any& data, WriteRequest::Ptr upperStreamReq) :
    data(data), action(upperStreamReq->action) {

}

WriteRequest::~WriteRequest() {

}

void WriteRequest::onComplete() {
  if (action) {
    action();
  }
}

boost::any& WriteRequest::getData() {
  return data;
}

Pipeline::Pipeline(boost::asio::io_service& ioService) :
    session(), BUFFER_SIZE(4096), readBuffer(), ioService(
        ioService), READ_TIMEOUT_SECS(30), WRITE_TIMEOUT_SECS(30),
		readTimer(ioService), writeTimer(ioService), sessionClosed(false),
		readCompleted(false) {
    }

Pipeline::~Pipeline() {
}

void Pipeline::addLast(CodecPtr codec) {
  ContextPtr ctx(new Context(*this));
  CodecContext codecContext(ctx, codec);
  codecContexts.push_back(codecContext);
}

void Pipeline::remove(CodecPtr codec) {
  codecContexts.erase(
      std::remove_if(codecContexts.begin(), codecContexts.end(),
                     [&codec] (CodecContext& codecContext) {
                     return (codecContext.get<1>() == codec);
                     }));
}

void Pipeline::setSession(SessionPtr ssn) {
	session = ssn;
}

void Pipeline::setHandler(HandlerPtr handler) {
  ContextPtr ctx(new Context(*this));
  handlerContext = HandlerContext(ctx, handler);
}

void Pipeline::write(boost::any& output) {
  //boost::mutex::scoped_lock lock(mutex);
  std::list<boost::any> in;

  in.push_back(output);

  std::list<boost::any> out;

  for (auto dc = codecContexts.rbegin(); dc != codecContexts.rend(); ++dc) {
    for (auto it = in.begin(); it != in.end(); ++it) {
      dc->get<1>()->encode(*dc->get<0>(), *it, out);
    }
    in = out;out.clear();
  }
  enqueueWriteRequest(in);
}

void Pipeline::setBufferSize(std::size_t bufferSize) {
  BUFFER_SIZE = bufferSize;
}

void Pipeline::setReadTimeout(std::size_t seconds) {
  READ_TIMEOUT_SECS = seconds;
}

void Pipeline::setWriteTimeout(std::size_t seconds) {
  WRITE_TIMEOUT_SECS = seconds;
}

void Pipeline::close() {
  session->close();
}

boost::asio::io_service& Pipeline::getIoService() {
  return ioService;
}

void Pipeline::read() {
  // 2.start reading.
  readCompleted = false;
  session->read(readBuffer.prepare(BUFFER_SIZE),
                boost::bind(&Pipeline::onReadComplete, shared_from_this(), _1,
                            _2));
}

void Pipeline::dequeueWriteRequest() {
  // 2.start writing.
  //boost::mutex::scoped_lock lock(mutex);
	while(true) {
	  if(!sessionClosed && !writeRequestQueue.empty()) {
		boost::any& any = writeRequestQueue.front()->getData();
		if (any.type() == typeid(boost::asio::streambuf*)) {
		  boost::asio::streambuf* buffer = boost::any_cast<
			  boost::asio::streambuf*>(any);
		  session->write(buffer->data(),
						 boost::bind(&Pipeline::onWriteComplete,
									 shared_from_this(), _1, _2));
		  return;
		} else {
		  // invalid data!
		  writeRequestQueue.pop();
		}
	  } else if(!sessionClosed && writeRequestQueue.empty()) {
		  // nothing to write!
		  waitWriteRequest();
		  //read();
		  return;
	  } else {
		  // session closed.
		  return;
	  }
	}

}

void Pipeline::enqueueWriteRequest(std::list<boost::any>& out) {
	if(!sessionClosed) {
	  for (auto it = out.begin(); it != out.end(); ++it) {
		WriteRequest::Ptr request(new WriteRequest(*it));
		writeRequestQueue.push(request);
	  }
	  // notifiy sender.
	  if (!out.empty()) {
		  notifyWriter();
	  } else {
	  }
	} else {
	}
}

void Pipeline::enqueueWriteRequest(std::list<boost::any>& out,
                                       CompletionHandler) {
  assert(false);
}

void Pipeline::waitWriteRequest() {
	writeTimer.expires_from_now(boost::posix_time::seconds(WRITE_TIMEOUT_SECS));
	writeTimer.async_wait(boost::bind(&Pipeline::onWriterTimeout, shared_from_this(), _1));
}

void Pipeline::waitReadResponse() {
	readTimer.expires_from_now(boost::posix_time::seconds(READ_TIMEOUT_SECS));
	readTimer.async_wait(boost::bind(&Pipeline::onReaderTimeout, shared_from_this(), _1));
}

void Pipeline::notifyWriter() {
	writeTimer.cancel();
}

void Pipeline::notifyReader() {
	readTimer.cancel();
}

void Pipeline::start() {
  // 1.notify encoders/decoders that session has commenced.
  processSessionStart();
  // 2.start reading && writing.
  read();
  dequeueWriteRequest();
  // 3.start event dispatching.
}

void Pipeline::onReadComplete(const boost::system::error_code& ec,
                                  std::size_t bytesTransfered) {
  readCompleted = true;
  if (!ec) {
	  if(bytesTransfered != 0) {
		readBuffer.commit(bytesTransfered);
		processDataArrive();
	  }
//	  if(!sessionClosed) {
//		  read();
//	  }
  } else if (ec.value() == boost::asio::error::eof
		  || ec.value() == boost::asio::error::broken_pipe
		  || ec.value() == boost::asio::error::connection_reset
		  || ec.value() == boost::asio::error::connection_aborted) {
	  sessionClosed = true;
	  processSessionClose();
  } else {
    processExceptionCaught(ec);
  }
}

void Pipeline::onWriteComplete(const boost::system::error_code& ec,
                                   std::size_t bytesTransfered) {

  if (!ec) {
      boost::any& any = writeRequestQueue.front()->getData();
      boost::asio::streambuf* buffer = boost::any_cast<boost::asio::streambuf*>(any);
      buffer->consume(bytesTransfered);

      if(buffer->size() == 0) {
   	    writeRequestQueue.front()->onComplete();
        writeRequestQueue.pop();
        /*
         * to avoid to much request that generate too much response
         * to write, move read call to this location instead of
         * an endless cycle of reads.
         */
        if(!sessionClosed
        		&& writeRequestQueue.empty()
				&& readCompleted) {
          // nothing to write!
          read();
        }
      }
      dequeueWriteRequest();
  } else if (ec.value() == boost::asio::error::eof
		  || ec.value() == boost::asio::error::broken_pipe
		  || ec.value() == boost::asio::error::connection_reset
		  || ec.value() == boost::asio::error::connection_aborted) {
	  sessionClosed = true;
	  processSessionClose();
  } else {
    processExceptionCaught(ec);
  }
}

void Pipeline::onReaderTimeout(const boost::system::error_code& ec) {
  if (!ec) {
	  if (!sessionClosed) {
		  processTimeout();
	  } else {
		  //session.reset();
	  }
  } else {
    processExceptionCaught(ec);
  }
}

void Pipeline::onWriterTimeout(const boost::system::error_code& ec) {
  if (!ec) {
    processTimeout();
  } else if (ec.value() == boost::asio::error::operation_aborted) {
    //boost::system::errc::operation_canceled
    if(!sessionClosed) {
    	dequeueWriteRequest();
    } else {
    	//session.reset();
    }
  } else {
    processExceptionCaught(ec);
  }
}

void Pipeline::processSessionStart() {
  std::for_each(codecContexts.begin(), codecContexts.end(),
                [] (CodecContext& codecContext) {
                codecContext.get<1>()->sessionStart(*codecContext.get<0>());
                });
  handlerContext.get<1>()->sessionStart(
		  *handlerContext.get<0>()
		  );

  writeBackContextQueues();
}

void Pipeline::processSessionClose() {
	if(session) {
	  notifyWriter();
	  notifyReader();
	  std::for_each(codecContexts.begin(), codecContexts.end(),
					[] (CodecContext& codecContext) {
					codecContext.get<1>()->sessionClose(*codecContext.get<0>());
					});
	  handlerContext.get<1>()->sessionClose(
			  *handlerContext.get<0>()
			  );
	  session.reset();
	}
}

void Pipeline::processExceptionCaught(const boost::system::error_code& ec) {
  //TODO: add error code to exception transformation.
  std::exception ex;
  std::for_each(codecContexts.begin(), codecContexts.end(),
                [ex] (CodecContext& codecContext) {
                codecContext.get<1>()->exceptionCaught(*codecContext.get<0>(), ex);
                });
  handlerContext.get<1>()->exceptionCaught(
		  *handlerContext.get<0>(),
		  ex
		  );
}

void Pipeline::writeBackContextQueues() {
	  std::list<boost::any> in;
	  std::list<boost::any> out;

	// check possible write backs.
	in = handlerContext.get<0>()->getOutputs();
	handlerContext.get<0>()->getOutputs().clear();
	// call decoders.
	for (auto dc = codecContexts.rbegin(); dc != codecContexts.rend(); ++dc) {
		in.insert(dc->get<0>()->getOutputs().begin(),
				dc->get<0>()->getOutputs().end());
		dc->get<0>()->getOutputs().clear();
		for (auto it = in.begin(); it != in.end(); ++it) {
			dc->get<1>()->encode(*dc->get<0>(), *it, out);
		}
		in = out;
		out.clear();
	}
	enqueueWriteRequest(in);
}

void Pipeline::processDataArrive() {
  std::list<boost::any> in;

  in.push_back(&readBuffer);

  std::list<boost::any> out;

  // call decoders.
  for (auto dc = codecContexts.begin(); dc != codecContexts.end(); ++dc) {
    for (auto it = in.begin(); it != in.end(); ++it) {
      dc->get<1>()->decode(*dc->get<0>(), *it, out);
    }
    in = out;
    out.clear();
  }
  // call handlers.
  for (auto it = in.begin(); it != in.end(); ++it) {
    handlerContext.get<1>()->handle(*handlerContext.get<0>(), *it);
  }

  // check possible write backs.
	writeBackContextQueues();
}

void Pipeline::processTimeout() {
  TimeoutException ex;
  std::for_each(codecContexts.begin(), codecContexts.end(),
                [ex] (CodecContext& codecContext) {
                codecContext.get<1>()->exceptionCaught(*codecContext.get<0>(), ex);
                });
}

Context::Context(Pipeline& p) :
    pipeline(p) {

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

Codec::Codec(const EncodeFunc ef, DecodeFunc df, const SessionStart s,
             const SessionClose c, const ExceptionCaught e) :
    encode(ef), decode(df), sessionStart(s), sessionClose(c), exceptionCaught(
        e) {

    }

Codec::~Codec() {

}

Handler::Handler(const HandlerFunc f, const SessionStart s,
                 const SessionClose c, const ExceptionCaught e) :
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
