#include <boost/test/unit_test.hpp>
#include <boost/asio.hpp>
#include <iostream>
#include "Codec.h"
#include "MockSession.h"

using namespace std;
using namespace codec;
using namespace boost;
using namespace boost::asio;

BOOST_AUTO_TEST_SUITE( SessionTest )

MockSession::Ptr mockSession(new MockSession());

BOOST_AUTO_TEST_CASE( testReadWrite ) {

	bool readComplete = false;
	bool writeComplete = false;

	char wbuff[] { (char)0xca, (char)0xfe, (char)0xba, (char)0xbe,
			(char)0xff, (char)0x00, (char)0x7e };

	SessionPtr ssn = mockSession->getSession();

	boost::asio::streambuf sb;
	std::ostream os(&sb);
	os.write(wbuff, sizeof(wbuff));

	BOOST_CHECK_EQUAL(sizeof(wbuff), sb.size());

	ssn->write(sb.data(), [&sb, &wbuff, &writeComplete] (
			const boost::system::error_code& ec,
			std::size_t bytesTransferred) {
		writeComplete = true;
		BOOST_CHECK_EQUAL(sizeof(wbuff), bytesTransferred);
		sb.consume(bytesTransferred);
		BOOST_CHECK_EQUAL(0, sb.size());
	});

	ssn->read(sb.prepare(sizeof(wbuff)), [&sb, &wbuff, &readComplete](
			const boost::system::error_code& ec,
			std::size_t bytesTransferred) {
		readComplete = true;
		BOOST_CHECK_EQUAL(sizeof(wbuff), bytesTransferred);
		sb.commit(bytesTransferred);
		BOOST_CHECK_EQUAL(sizeof(wbuff), sb.size());
	});

	try {
		mockSession->run();
	} catch (std::exception& ex) {
		BOOST_CHECK(false);
		cout << ex.what() << endl;
	}

	BOOST_CHECK_EQUAL(readComplete, true);
	BOOST_CHECK_EQUAL(writeComplete, true);

	boost::asio::streambuf::const_buffers_type cbufs = sb.data();
	const_buffer cb = *(cbufs.begin());
	std::size_t csize = boost::asio::detail::buffer_size_helper(cb);
	const char* cdata = reinterpret_cast<const char*>(
			boost::asio::detail::buffer_cast_helper(cb));

	BOOST_CHECK(std::equal(cdata, (cdata + csize), wbuff));

	sb.consume(csize);
	BOOST_CHECK_EQUAL(0, sb.size());
}

BOOST_AUTO_TEST_SUITE_END()

