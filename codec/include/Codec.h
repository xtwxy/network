/*
 * Codec.h
 *
 *  Created on: Apr 22, 2016
 *      Author: master
 */

#ifndef INCLUDE_CODEC_H_
#define INCLUDE_CODEC_H_

#include <list>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/noncopyable.hpp>
#include <boost/endian/buffers.hpp>
#include <boost/function.hpp>
#include <boost/system/error_code.hpp>
#include <boost/any.hpp>

namespace codec {

class Session;
class Encoder;
class Decoder;
class Handler;
class Context;

typedef boost::shared_ptr<Session> SessionPtr;
typedef boost::shared_ptr<Encoder> EncoderPtr;
typedef boost::shared_ptr<Decoder> DecoderPtr;
typedef boost::shared_ptr<Handler> HandlerPtr;
typedef boost::shared_ptr<Context> ContextPtr;

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

typedef boost::function<void (Context&)> SessionClose;
typedef boost::function<void (Context&, std::exception&)> ExceptionCaught;

class Pipeline :private boost::noncopyable{
public:
	Pipeline(SessionPtr ssn);
	virtual ~Pipeline();

	void addLast(EncoderPtr encoder);
	void addLast(DecoderPtr decoder);
	void remove(EncoderPtr encoder);
	void remove(DecoderPtr decoder);
	void setHandler(HandlerPtr handler);
	void close();
	void start();
private:
	SessionPtr session;
	std::list<EncoderPtr> encoders;
	std::list<DecoderPtr> decoders;
	HandlerPtr handler;
};

typedef boost::function<void (Pipeline&)>PipelineInitializer;

class Context {
public:
	Context(Pipeline& p);
	virtual ~Context();

	Pipeline& getPipeline();
	void write(boost::any&);
	void close();
private:
	Pipeline& pipeline;
};

class Encoder : public boost::enable_shared_from_this<Encoder>,
private boost::noncopyable {
public:
	Encoder(const EncodeFunc,
			const SessionClose,
			const ExceptionCaught);
	virtual ~Encoder();

	const EncodeFunc encode;
	const SessionClose sessionClose;
	const ExceptionCaught exceptionCaught;
};

class Decoder : public boost::enable_shared_from_this<Decoder>,
private boost::noncopyable {
public:
	Decoder(const DecodeFunc,
			const SessionClose,
			const ExceptionCaught);
	virtual ~Decoder();

	const DecodeFunc decode;
	const SessionClose sessionClose;
	const ExceptionCaught exceptionCaught;
};

class Handler : public boost::enable_shared_from_this<Handler>,
private boost::noncopyable {
public:
	Handler(const HandlerFunc,
			const SessionClose,
			const ExceptionCaught);
	virtual ~Handler();

	const HandlerFunc handle;
	const SessionClose sessionClose;
	const ExceptionCaught exceptionCaught;
};

class Session: public boost::enable_shared_from_this<Session>,
		public boost::noncopyable {
public:
	typedef boost::function<
			void (const boost::system::error_code&, std::size_t)> IoCompHandler;
	typedef boost::function<void (char*, std::size_t, IoCompHandler)> Read;
	typedef boost::function<void (char*, std::size_t, IoCompHandler)> Write;
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
