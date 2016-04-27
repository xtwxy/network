#ifndef INCLUDE_MOCK_SESSION_IPP_
#define INCLUDE_MOCK_SESSION_IPP_

	codec::Session::Read read = [&readBuffer, &bytesRead] (
			char* buff,
			std::size_t len,
			codec::Session::IoCompHandler handler) {
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

	codec::Session::Write write = [&writeBuffer, &bytesWritten] (
			char* buff,
			std::size_t len,
			codec::Session::IoCompHandler handler) {
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

	codec::Session::Post post = [&ioService] (codec::Session::Task t) {
		ioService.post(t);
	};
	
	codec::Session::Close close = [] () {
		BOOST_ERROR("Close is not implemented.");
	};

#endif /* INCLUDE_MOCK_SESSION_IPP_ */
