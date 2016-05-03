/*
 * MockTrivialCodec.h
 *
 *  Created on: Apr 29, 2016
 *      Author: master
 */

#ifndef INCLUDE_MOCKTRIVIALCODEC_H_
#define INCLUDE_MOCKTRIVIALCODEC_H_

#include "Codec.h"

namespace codec {

class MockTrivialCodec: public boost::enable_shared_from_this<MockTrivialCodec>,
		private boost::noncopyable {
public:
	typedef boost::shared_ptr<MockTrivialCodec> Ptr;
	MockTrivialCodec();
	virtual ~MockTrivialCodec();

	static CodecPtr createCodec();
	CodecPtr getCodec();
	void encode(Context&, boost::any&, std::list<boost::any>&);
	void decode(Context&, boost::any&, std::list<boost::any>&);
	void sessionStart(Context&);
	void sessionClose(Context&);
	void exceptionCaught(Context&, const std::exception&);

	void checkPostCondition();
private:
	bool encodeCalled;
	bool decodeCalled;
	bool sessionStartCalled;
	bool sessionCloseCalled;
	bool exceptionCaughtCalled;
};

} /* namespace codec */

#endif /* INCLUDE_MOCKTRIVIALCODEC_H_ */
