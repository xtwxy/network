#include <boost/test/unit_test.hpp>
#include <boost/asio.hpp>
#include <boost/make_shared.hpp>
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

  MockTrivialCodec::Ptr layer1 = boost::make_shared<MockTrivialCodec>();
  MockTrivialCodec::Ptr layer2 = boost::make_shared<MockTrivialCodec>();  

  pipeline.addLast(layer1->getCodec());
	pipeline.addLast(layer2->getCodec());

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
	//layer1->checkPostCondition();
	//layer2->checkPostCondition();
}

BOOST_AUTO_TEST_SUITE_END()

