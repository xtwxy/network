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
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread/mutex.hpp>

namespace codec {

class Session;
class Codec;
class Handler;
class Context;
class Pipeline;

typedef boost::shared_ptr<Session> SessionPtr;
typedef boost::shared_ptr<Codec> CodecPtr;
typedef boost::shared_ptr<Handler> HandlerPtr;
typedef boost::shared_ptr<Context> ContextPtr;
typedef boost::shared_ptr<Pipeline> PipelinePtr;

typedef boost::tuple<ContextPtr, CodecPtr> CodecContext;
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

typedef boost::function<void (Pipeline&)> PipelineInitializer;

class TimeoutException : public std::exception {
public:
	TimeoutException();
	virtual ~TimeoutException();
};

class WriteRequest : public boost::enable_shared_from_this<WriteRequest>,
private boost::noncopyable {
public:
	typedef boost::shared_ptr<WriteRequest> Ptr;
	WriteRequest(boost::any& data);
	WriteRequest(boost::any& data, CompletionHandler);
	WriteRequest(boost::any& data, WriteRequest::Ptr upperStreamCmd);
	virtual ~WriteRequest();

	void onComplete();
	boost::any& getData();
private:
	boost::any data;
	CompletionHandler action;
};

class Pipeline : public boost::enable_shared_from_this<Pipeline>,
private boost::noncopyable {
public:
	typedef boost::shared_ptr<Pipeline> Ptr;
	Pipeline(boost::asio::io_service& ioService);
	virtual ~Pipeline();
	void addLast(CodecPtr encoder);
	void remove(CodecPtr encoder);
	void setSession(SessionPtr ssn);
	void setHandler(HandlerPtr handler);
	void write(boost::any&);
	void write(boost::any&, CompletionHandler);
	void setBufferSize(std::size_t bufferSize);
	void setReadTimeout(std::size_t seconds);
	void setWriteTimeout(std::size_t seconds);
	void close();
	boost::asio::io_service& getIoService();
	void start();
private:
	void onReadComplete(const boost::system::error_code&, std::size_t);
	void onWriteComplete(const boost::system::error_code&, std::size_t);
	void onReaderTimeout(const boost::system::error_code& e);
	void onWriterTimeout(const boost::system::error_code& e);
	void read();
	void dequeueWriteRequest();
	void enqueueWriteRequest(std::list<boost::any>&);
	void enqueueWriteRequest(std::list<boost::any>&, CompletionHandler);
	void waitWriteRequest();
	void waitReadResponse();
	void notifyWriter();
	void notifyReader();

	void processSessionStart();
	void processSessionClose();
	void processExceptionCaught(const boost::system::error_code& ec);
	void processDataArrive();
	void processTimeout();
	void writeBackContextQueues();

	SessionPtr session;
	std::list<CodecContext> codecContexts;
	HandlerContext handlerContext;
	std::size_t BUFFER_SIZE;
	std::size_t READ_TIMEOUT_SECS;
	std::size_t WRITE_TIMEOUT_SECS;
	boost::asio::deadline_timer readTimer;
	boost::asio::deadline_timer writeTimer;
	boost::asio::streambuf readBuffer;
	std::queue<WriteRequest::Ptr> writeRequestQueue;
	//boost::mutex mutex;
	boost::asio::io_service& ioService;
	bool sessionClosed;
	bool readCompleted;
};

class Context {
public:
	Context(Pipeline& p);
	virtual ~Context();

	Pipeline& getPipeline();
	void write(boost::any&);
	void write(boost::any&, CompletionHandler);
	void close();
	std::list<boost::any>& getOutputs();
private:
	Pipeline& pipeline;
	std::list<boost::any> outputs;
};

class Codec : public boost::enable_shared_from_this<Codec>,
private boost::noncopyable {
public:
	Codec(const EncodeFunc,
			const DecodeFunc,
			const SessionStart,
			const SessionClose,
			const ExceptionCaught);
	virtual ~Codec();

	const EncodeFunc encode;
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
	typedef boost::asio::streambuf::mutable_buffers_type ReadBuffer;
	typedef boost::asio::streambuf::const_buffers_type WriteBuffer;
	typedef boost::function<
			void (const boost::system::error_code&, std::size_t)> IoCompHandler;
	typedef boost::function<void (ReadBuffer, IoCompHandler)> Read;
	typedef boost::function<void (WriteBuffer, IoCompHandler)> Write;
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
