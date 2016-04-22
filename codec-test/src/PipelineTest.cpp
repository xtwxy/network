#include <boost/test/unit_test.hpp>
#include <boost/asio.hpp>

#include "Codec.h"

BOOST_AUTO_TEST_SUITE( PipelineTest )

BOOST_AUTO_TEST_CASE( testNoCodecs ) {

	boost::asio::io_service ioService;

	unsigned char expected[] {
			0xfa, 0x55, 0xfa, 0x55, 0xfa, 0x55,
			0x01,
			0x02,
			0x01,
			0x0b, 0x00,
			0x0f, 0x00,
			0xfd, 0x22, 0xfd, 0x22
	};

	unsigned char writeBuffer[17];
	unsigned char readBuffer[2048];
	std::size_t bytesWritten = 0;
	std::size_t bytesRead = 0;

	codec::Session::IoCompHandler readCompletion =
			[] (const boost::system::error_code& ec,
					std::size_t bytes_transfered) {
		BOOST_ERROR("Read completion cannot be happen.");
	};

	codec::Session::IoCompHandler writeCompletion =
			[&writeBuffer] (const boost::system::error_code& ec,
					std::size_t bytes_transfered) {
		BOOST_CHECK_EQUAL( true, false );
	};

#include "MockSession.ipp"

  codec::SessionPtr session(new codec::Session(read, write, post, close));
  codec::Pipeline pipeline(session);
  pipeline.start();
  //BOOST_CHECK_EQUAL(i, 10);

}

BOOST_AUTO_TEST_SUITE_END()

