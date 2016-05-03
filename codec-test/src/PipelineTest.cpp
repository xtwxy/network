#include <boost/test/unit_test.hpp>
#include <boost/asio.hpp>
#include <boost/make_shared.hpp>
#include <iostream>

#include "Codec.h"
#include "MockSession.h"
#include "MockTrivialCodec.h"
#include "MockHandler.h"

using namespace std;
using namespace codec;

BOOST_AUTO_TEST_SUITE( PipelineTest )


BOOST_AUTO_TEST_CASE( testCodec ) {
	MockSession::Ptr mockSession = boost::make_shared<MockSession>();
	MockHandler::Ptr mockHandler = boost::make_shared<MockHandler>();

	boost::function<void (bool)> checkTrue = [](bool b) {
		  	BOOST_CHECK_EQUAL(b, true);
	};

	boost::function<void (bool)> checkFalse = [](bool b) {
		  	BOOST_CHECK_EQUAL(b, false);
	};
  // initialize pipeline.
  Pipeline pipeline(mockSession->getIoService(), mockSession->getSession());

  MockTrivialCodec::Ptr layer1 = boost::make_shared<MockTrivialCodec>();
  MockTrivialCodec::Ptr layer2 = boost::make_shared<MockTrivialCodec>();  

  pipeline.addLast(layer1->getCodec());
  pipeline.addLast(layer2->getCodec());
  pipeline.setHandler(mockHandler->getHandler());
  pipeline.start();

  // prepare data to be written.
  bool readComplete = false;
  bool writeComplete = false;

  char wbuff[] { (char)0xca, (char)0xfe, (char)0xba, (char)0xbe,
    (char)0xff, (char)0x00, (char)0x7e };

  boost::asio::streambuf sb;
  std::ostream os(&sb);
  os.write(wbuff, sizeof(wbuff));

  BOOST_CHECK_EQUAL(sizeof(wbuff), sb.size());

  boost::any out = &sb;
  pipeline.write(out);

  try{
	  mockSession->getIoService().run();
  } catch(std::exception& ex) {
	  std::cerr << ex.what() << std::endl;
  }

	layer1->checkEncodeCalled([](bool encodeCalled) {
		BOOST_CHECK_EQUAL(encodeCalled, true);
	});
	layer1->checkDecodeCalled([](bool decodeCalled) {
		BOOST_CHECK_EQUAL(decodeCalled, true);
	});
	layer1->checkSessionStartCalled([](bool sessionStartCalled) {
		BOOST_CHECK_EQUAL(sessionStartCalled, true);
	});
	layer1->checkSessionCloseCalled([](bool sessionCloseCalled) {
		BOOST_CHECK_EQUAL(sessionCloseCalled, false);
	});
	layer1->checkExceptionCaughtCalled([](bool exceptionCaughtCalled) {
		BOOST_CHECK_EQUAL(exceptionCaughtCalled, true);
	});

	layer2->checkEncodeCalled([](bool encodeCalled) {
		BOOST_CHECK_EQUAL(encodeCalled, true);
	});
	layer2->checkDecodeCalled([](bool decodeCalled) {
		BOOST_CHECK_EQUAL(decodeCalled, true);
	});
	layer2->checkSessionStartCalled([](bool sessionStartCalled) {
		BOOST_CHECK_EQUAL(sessionStartCalled, true);
	});
	layer2->checkSessionCloseCalled([](bool sessionCloseCalled) {
		BOOST_CHECK_EQUAL(sessionCloseCalled, false);
	});
	layer2->checkExceptionCaughtCalled([](bool exceptionCaughtCalled) {
		BOOST_CHECK_EQUAL(exceptionCaughtCalled, true);
	});

	mockHandler->checkHandleCalled([](bool handleCalled) {
		BOOST_CHECK_EQUAL(handleCalled, true);
	});
	mockHandler->checkSessionStartCalled([](bool sessionStartCalled) {
		BOOST_CHECK_EQUAL(sessionStartCalled, true);
	});
	mockHandler->checkSessionCloseCalled([](bool sessionCloseCalled) {
		BOOST_CHECK_EQUAL(sessionCloseCalled, false);
	});
	mockHandler->checkExceptionCaughtCalled([](bool exceptionCaughtCalled) {
		BOOST_CHECK_EQUAL(exceptionCaughtCalled, true);
	});

}

BOOST_AUTO_TEST_SUITE_END()

