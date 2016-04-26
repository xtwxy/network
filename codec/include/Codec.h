/*
 * Codec.h
 *
 *  Created on: Apr 22, 2016
 *      Author: master
 */

#ifndef INCLUDE_CODEC_H_
#define INCLUDE_CODEC_H_

#include <list>
#include <queue>
#include <array>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/noncopyable.hpp>
#include <boost/endian/buffers.hpp>
#include <boost/function.hpp>
#include <boost/system/error_code.hpp>
#include <boost/any.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/asio.hpp>

namespace codec {

class Session;
class Encoder;
class Decoder;
class Handler;
class Context;
class Pipeline;

typedef boost::shared_ptr<Session> SessionPtr;
typedef boost::shared_ptr<Encoder> EncoderPtr;
typedef boost::shared_ptr<Decoder> DecoderPtr;
typedef boost::shared_ptr<Handler> HandlerPtr;
typedef boost::shared_ptr<Context> ContextPtr;

typedef boost::tuple<ContextPtr, EncoderPtr> EncoderContext;
typedef boost::tuple<ContextPtr, DecoderPtr> DecoderContext;
typedef boost::tuple<ContextPtr, HandlerPtr> HandlerContext;

typedef boost::function<
		void (Context&,
				boost::any&,
				std::list<boost::any>&)> EncodeFunc;

typedef boost::function<
		void (Context&,
				boost::any&,
				std::list<boost::any>&)> DecodeFunc;

typedef boost::function<
		void (Context&,
				boost::any&)> HandlerFunc;

typedef boost::function<void ()> CompletionHandler;

typedef boost::function<void (Context&)> SessionStart;
typedef boost::function<void (Context&)> SessionClose;
typedef boost::function<void (Context&, const std::exception&)> ExceptionCaught;

class TimeoutException : public std::exception {
public:
	TimeoutException();
	virtual ~TimeoutException();
};

class WriteRequest : public boost::enable_shared_from_this<WriteRequest>,
private boost::noncopyable {
public:
	typedef boost::shared_ptr<WriteRequest> Ptr;
	typedef boost::function<void ()> CompleteAction;
	WriteRequest();
	WriteRequest(CompleteAction);
	WriteRequest(WriteRequest::Ptr upperStreamCmd);
	virtual ~WriteRequest();

	void onComplete();

	boost::any data;
	CompleteAction action;
};

class PipelineImpl : public boost::enable_shared_from_this<PipelineImpl>,
private boost::noncopyable {
public:
	PipelineImpl(Pipeline& p, SessionPtr ssn, const std::size_t bufferSize=1024);
	virtual ~PipelineImpl();
	void addLast(EncoderPtr encoder);
	void addLast(DecoderPtr decoder);
	void remove(EncoderPtr encoder);
	void remove(DecoderPtr decoder);
	void setHandler(HandlerPtr handler);
	void close();
	void start();
private:
	void onReadComplete(const boost::system::error_code&, std::size_t);
	void onWriteComplete(const boost::system::error_code&, std::size_t);
	void onTimeout(const boost::system::error_code& e);
	void read();
	void write();

	void processSessionStart();
	void processSessionClose();
	void processExceptionCaught(const boost::system::error_code& ec);
	void processDataArrive();
	void processTimeout();

	Pipeline& parent;
	SessionPtr session;
	std::list<EncoderContext> encoderContexts;
	std::list<DecoderContext> decoderContexts;
	HandlerContext handlerContext;
	const std::size_t BUFFER_SIZE;
	boost::asio::streambuf readBuffer;
	std::queue<WriteRequest::Ptr> writeRequestQueue;
};

class Pipeline :private boost::noncopyable {
public:
	Pipeline(SessionPtr ssn, const std::size_t bufferSize=1024);
	virtual ~Pipeline();

	void addLast(EncoderPtr encoder);
	void addLast(DecoderPtr decoder);
	void remove(EncoderPtr encoder);
	void remove(DecoderPtr decoder);
	void setHandler(HandlerPtr handler);
	void close();
private:
	PipelineImpl impl;
};

typedef boost::function<void (Pipeline&)>PipelineInitializer;

class Context {
public:
	Context(Pipeline& p);
	virtual ~Context();

	Pipeline& getPipeline();
	void write(boost::any&);
	void write(boost::any&, CompletionHandler);
	void close();
	std::list<boost::any>& getOut();
private:
	Pipeline& pipeline;
	std::list<boost::any> out;
};

class Encoder : public boost::enable_shared_from_this<Encoder>,
private boost::noncopyable {
public:
	Encoder(const EncodeFunc,
			const SessionStart,
			const SessionClose,
			const ExceptionCaught);
	virtual ~Encoder();

	const EncodeFunc encode;
	const SessionStart sessionStart;
	const SessionClose sessionClose;
	const ExceptionCaught exceptionCaught;
};

class Decoder : public boost::enable_shared_from_this<Decoder>,
private boost::noncopyable {
public:
	Decoder(const DecodeFunc,
			const SessionStart,
			const SessionClose,
			const ExceptionCaught);
	virtual ~Decoder();

	const DecodeFunc decode;
	const SessionStart sessionStart;
	const SessionClose sessionClose;
	const ExceptionCaught exceptionCaught;
};

class Handler : public boost::enable_shared_from_this<Handler>,
private boost::noncopyable {
public:
	Handler(const HandlerFunc,
			const SessionStart,
			const SessionClose,
			const ExceptionCaught);
	virtual ~Handler();

	const HandlerFunc handle;
	const SessionStart sessionStart;
	const SessionClose sessionClose;
	const ExceptionCaught exceptionCaught;
};

class Session: public boost::enable_shared_from_this<Session>,
		public boost::noncopyable {
public:
	typedef boost::asio::streambuf::mutable_buffers_type InBuffer;
	typedef boost::asio::streambuf::const_buffers_type OutBuffer;
	typedef boost::function<
			void (const boost::system::error_code&, std::size_t)> IoCompHandler;
	typedef boost::function<void (InBuffer, IoCompHandler)> Read;
	typedef boost::function<void (OutBuffer, IoCompHandler)> Write;
	typedef boost::function<void ()> Close;
	typedef boost::function<void ()> Task;
	typedef boost::function<void (Task)> Post;

	Session(Read r, Write w, Post p, Close c);

	virtual ~Session();

	const Read read;
	const Write write;
	const Close close;
	const Post post;
};

} /* namespace codec */

#endif /* INCLUDE_CODEC_H_ */
