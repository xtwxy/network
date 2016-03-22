#include <cstdlib>
#include <iostream>
#include <map>
#include <string>
#include <boost/asio.hpp>
#include <boost/asio/buffer.hpp>

const static int BUFFER_SIZE = 4096;

char buffer[BUFFER_SIZE];
void read() {
	async_write(
			socket_,
			boost::asio::buffer(readBuffer_, BUFFER_SIZE),
			onReadComplete
	);
}
void write() {
	async_write(
			socket_,
			boost::asio::buffer(writeBuffer_, BUFFER_SIZE),
			boost::bind(
					&Connection::onWriteComplete,
					shared_from_this(),
					_1,
					_2
					)
	);
}

void onReadComplete(const boost::system::error_code& ec,
		size_t bytes_transferred) {
	if (!ec) {
	} else {
		std::cerr << ec << std::endl;
	}

}
void onWriteComplete(const boost::system::error_code& ec,
		size_t bytes_transferred) {
	if (!ec) {
	} else {
		std::cerr << ec << std::endl;
	}
}

int main(int argc, char* argv[]) {
	boost::asio::io_service ios;

	ios.run();
	return EXIT_SUCCESS;
}

