#include <cstdlib>
#include <iostream>
#include <map>
#include <string>
#include <boost/asio.hpp>
#include <boost/asio/buffer.hpp>

const static int BUFFER_SIZE = 4096;

char readBuffer[BUFFER_SIZE];
char writeBuffer[BUFFER_SIZE];

boost::asio::io_service ios;
boost::asio::ip::tcp::resolver resolver(ios);
boost::asio::ip::tcp::socket sock(ios);

void onReadComplete(const boost::system::error_code& ec,
		size_t bytes_transferred);
void onWriteComplete(const boost::system::error_code& ec,
		size_t bytes_transferred);

void read() {
	async_write(
			sock,
			boost::asio::buffer(readBuffer, BUFFER_SIZE),
			onReadComplete
	);
}
void write() {
	async_write(
			sock,
			boost::asio::buffer(writeBuffer, BUFFER_SIZE),
			onWriteComplete
	);
}

void onConnectComplete(
	const boost::system::error_code& ec,
	boost::asio::ip::tcp::resolver::iterator i) {
	if (!ec) {
		read();
		write();
	} else {
	}
}

void onResolveComplete(
	const boost::system::error_code& ec,
	boost::asio::ip::tcp::resolver::iterator i) {
	if (!ec) {
		boost::asio::ip::tcp::resolver::iterator end;
		boost::asio::async_connect(
				sock,
				i,
				end,
				onConnectComplete
			);
	} else {
	}
}

void resolve(std::string host, std::string port) {
	boost::asio::ip::tcp::resolver::query q(host, port);
	resolver.async_resolve(
			q,
			onResolveComplete
	);
}

void onReadComplete(const boost::system::error_code& ec,
		size_t bytes_transferred) {
	if (!ec) {
		read();
	} else {
		std::cerr << ec << std::endl;
	}

}
void onWriteComplete(const boost::system::error_code& ec,
		size_t bytes_transferred) {
	if (!ec) {
		write();
	} else {
		std::cerr << ec << std::endl;
	}
}

int main(int argc, char* argv[]) {
	if(argc != 3) {
		std::cerr << "Usage:\n"
			<< argv[0] << "<host> <port>"
			<< std::endl; 
			return EXIT_FAILURE;
	}
	for(std::size_t i = 0; i != BUFFER_SIZE; ++i) {
		readBuffer[i] = i;
		writeBuffer[i]	= i;
	}
	resolve(argv[1], argv[2]);
	ios.run();
	return EXIT_SUCCESS;
}

