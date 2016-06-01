/*
 * MessageCodec.h
 *
 *  Created on: May 26, 2016
 *      Author: master
 */

#ifndef INCLUDE_MESSAGECODEC_H_
#define INCLUDE_MESSAGECODEC_H_

#include "Codec.h"
#include "CallProtocol.h"

namespace CallProtocol {

class MessageCodec : public boost::enable_shared_from_this<MessageCodec>,
private boost::noncopyable {
public:

	typedef boost::shared_ptr<MessageCodec> Ptr;
	typedef boost::function<bool (codec::Context&, boost::any&,  std::list<boost::any>&)> Action;

	MessageCodec(PayloadFactoryPtr);
	virtual ~MessageCodec();

	static codec::CodecPtr createCodec(PayloadFactoryPtr);
	codec::CodecPtr getCodec();
	void encode(codec::Context&, boost::any&, std::list<boost::any>&);
	void decode(codec::Context&, boost::any&, std::list<boost::any>&);
	void sessionStart(codec::Context&);
	void sessionClose(codec::Context&);
	void exceptionCaught(codec::Context&, const std::exception&);
private:
	bool decodeHeader(codec::Context& ctx, boost::any& input, std::list<boost::any>& output);
	bool decodeBody(codec::Context& ctx, boost::any& input, std::list<boost::any>& output);

	MessagePtr message;
	Action action;
	PayloadFactoryPtr payloadFactory;
};

} /* namespace CallProtocol */

#endif /* INCLUDE_MESSAGECODEC_H_ */
