#include <boost/test/unit_test.hpp>
#include <boost/asio.hpp>
#include <iostream>

#include "Codec.h"
#include "MockSession.h"
#include "MockTrivialCodec.h"

using namespace std;
using namespace codec;

BOOST_AUTO_TEST_SUITE( PipelineTest )

MockSession::Ptr mockSession(new MockSession());

BOOST_AUTO_TEST_CASE( testInitialize ) {
	// initialize pipeline.
	Pipeline pipeline(mockSession->getSession());
	pipeline.addLast(MockTrivialCodec::getCodec());
	pipeline.addLast(MockTrivialCodec::getCodec());

	// prepare data to be written.
	bool readComplete = false;
	bool writeComplete = false;

	char wbuff[] { (char)0xca, (char)0xfe, (char)0xba, (char)0xbe,
			(char)0xff, (char)0x00, (char)0x7e };

	SessionPtr ssn = mockSession->getSession();

	boost::asio::streambuf sb;
	std::ostream os(&sb);
	os.write(wbuff, sizeof(wbuff));

	BOOST_CHECK_EQUAL(sizeof(wbuff), sb.size());

	boost::any out = &sb;
	pipeline.write(out);
}

BOOST_AUTO_TEST_SUITE_END()

