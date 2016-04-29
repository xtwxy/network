#include <cstdlib>
#include <iostream>
#include <map>
#include <string>
#include <boost/asio.hpp>
#include <boost/asio/buffer.hpp>

static int BUFFER_SIZE = 4096;

boost::asio::streambuf buffer;

boost::asio::io_service ios;
boost::asio::ip::tcp::resolver resolver(ios);
boost::asio::ip::tcp::socket sock(ios);

void onReadComplete(const boost::system::error_code& ec,
		size_t bytes_transferred);
void onWriteComplete(const boost::system::error_code& ec,
		size_t bytes_transferred);

void read() {
	sock.async_read_some(
			buffer.prepare(BUFFER_SIZE),
			onReadComplete
	);
}
void write() {
    if(buffer.size() == 0) {
        std::ostream os(&buffer);
        os << "Hello, World!\n";

        boost::asio::mutable_buffer b;
        std::size_t size = boost::asio::detail::buffer_size_helper(b);
        void * data = boost::asio::detail::buffer_cast_helper(b);
      async_write(
          sock,
          buffer.data(),
          onWriteComplete
          );

    } else {
      async_write(
          sock,
          buffer.data(),
          onWriteComplete
          );
	}
}

void onConnectComplete(
	const boost::system::error_code& ec,
	boost::asio::ip::tcp::resolver::iterator i) {
	if (!ec) {
		read();
		write();
	} else {
		std::cerr << "onConnectComplete(): " << ec << std::endl;
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
		std::cerr << "onResolveComplete(): " << ec << std::endl;
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
    buffer.commit(bytes_transferred);
		read();
	} else {
		std::cerr << "onReadComplete(): " << ec << std::endl;
	}

}
void onWriteComplete(const boost::system::error_code& ec,
		size_t bytes_transferred) {
	if (!ec) {
    buffer.consume(bytes_transferred);
		write();
	} else {
		std::cerr << "onWriteComplete(): " << ec << std::endl;
	}
}

int main(int argc, char* argv[]) {
	if(argc < 3) {
		std::cerr << "Usage:\n"
			<< argv[0] << " <host> <port>"
			<< std::endl; 
			return EXIT_FAILURE;
	} else if(argc == 4) {
		BUFFER_SIZE = atoi(argv[3]);
	}

    resolve(argv[1], argv[2]);

	ios.run();


	return EXIT_SUCCESS;
}

