#include <iostream>
#include <sstream>
#include <boost/test/unit_test.hpp>
#include <random>
#include <algorithm>

#include "Command.h"

BOOST_AUTO_TEST_SUITE(battery);


BOOST_AUTO_TEST_CASE( battery_CommandEncodeTest  ) {
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

	battery::Session::Handler readCompletion =
			[] (const boost::system::error_code& ec,
					std::size_t bytes_transfered) {
		BOOST_ERROR("Read completion cannot be happen.");
	};

	battery::Session::Handler writeCompletion =
			[&writeBuffer] (const boost::system::error_code& ec,
					std::size_t bytes_transfered) {
		BOOST_CHECK_EQUAL( true, false );
	};

	battery::Session::Read read = [&readBuffer, &bytesRead] (
			char* buff,
			std::size_t len,
			battery::Session::Handler handler) {
		std::size_t toRead = len;
		std::size_t available = (sizeof(writeBuffer) - bytesRead);

		BOOST_REQUIRE(len <= available);

		if(len > available) {
			toRead = available;
		}

		std::copy(buff, (buff + toRead), readBuffer);
		bytesRead += toRead;

		handler(
			boost::system::errc::make_error_code(
				boost::system::errc::bad_message
			),
			bytesRead
		);
	};

	battery::Session::Write write = [&writeBuffer, &bytesWritten] (
			char* buff,
			std::size_t len,
			battery::Session::Handler handler) {
		std::size_t toWrite = len;
		std::size_t available = sizeof(writeBuffer);

		BOOST_REQUIRE(len <= available);

		if(len > available) {
			toWrite = available;
		}

		std::copy(buff, (buff + toWrite), writeBuffer);
		bytesWritten += toWrite;

		handler(
			boost::system::errc::make_error_code(
				boost::system::errc::bad_message
			),
			bytesWritten
		);
	};

	battery::Session::Close close = [] () {
		BOOST_ERROR("Close is not implemented.");
	};

	battery::Session::Ptr session(new battery::Session(read, write, close));

	battery::Command::Encoder encoder = [] (
			char * const buff,                        // Output writeBuffer
            std::size_t size,                         // Length of bytes encoded
            boost::endian::little_uint8_buf_t& addr,  // Addr
            boost::endian::little_uint8_buf_t& packNo,// PackNo
            boost::endian::little_uint8_buf_t& cmd,   // Cmd
            boost::endian::little_uint16_buf_t& len   // Len
           ) {

		std::size_t dataSize = 0;

		addr 	= 0x01;
		packNo 	= 0x02;
		cmd 	= 0x01;
		len		= len.value() + dataSize;

		return true;
	};

	battery::Command::Decoder decoder = [] (
			char * const buff,                        // Output writeBuffer
            std::size_t size,                         // Length of bytes encoded
            boost::endian::little_uint8_buf_t& addr,  // Addr
            boost::endian::little_uint8_buf_t& packNo,// PackNo
            boost::endian::little_uint8_buf_t& cmd,   // Cmd
            boost::endian::little_uint16_buf_t& len   // Len
           ) {

		std::size_t dataSize = 0;

		addr 	= 0x01;
		packNo 	= 0x02;
		cmd 	= 0x03;
		len		= len.value() + dataSize;

		return true;
	};

	battery::Command::CompletionHandler completionHandler = [](
			const boost::system::error_code& ec) {
		BOOST_CHECK( !ec );
		BOOST_CHECK_EQUAL( true, false );
	};

	battery::Command::Ptr cmd(
			new battery::Command(
				session,
				encoder,
				decoder,
				completionHandler
			)
	);

	cmd->execute();

	for(std::size_t i = 0; i != bytesWritten; ++i) {
		BOOST_CHECK_EQUAL( expected[i], writeBuffer[i]);
		printf("%2.2x ", (0xff & writeBuffer[i]));
	}
	printf("\n");
}

BOOST_AUTO_TEST_SUITE_END();
