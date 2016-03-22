/*
 * Acceptor.h
 *
 *  Created on: Mar 22, 2016
 *      Author: master
 */

#ifndef INCLUDE_NW_ACCEPTOR_HPP_
#define INCLUDE_NW_ACCEPTOR_HPP_

#include <boost/asio.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <nw/LogFilter.h>
#include <nw/CodecFactory.h>


namespace nw {

void print_buffer(const char* const buffer, const std::size_t length, const char* comment = "[bytes] ") {
	printf("%s: ", comment);
	for (std::size_t i = 0; i < length; ++i) {
		printf("%2.2x ", (0xff & buffer[i]));
	}
	printf("\n");
}

class Connection : public boost::enable_shared_from_this<Connection> {
public:
	typedef boost::shared_ptr<Connection> Ptr;
	Connection(boost::asio::io_service& ios,
			std::size_t bufferSize,
			std::size_t timeoutSecs)
		:ios_(ios),
		 socket_(ios),
		 BUFFER_SIZE(bufferSize),
		 readBuffer_(new char[bufferSize]),
		 writeBuffer_(new char[bufferSize]),
		 TIMEOUT_SECONDS(timeoutSecs),
		 bytesToWrite(0),
		 bytesRead(0) { }
	virtual ~Connection() {
		std::cerr << "Connection::~Connection()" << std::endl;
		socket_.close();
		delete[] readBuffer_;
		delete[] writeBuffer_;
	}

	static Ptr newConnection(boost::asio::io_service& ios, std::size_t bufferSize, std::size_t timeoutSecs) {
		return Ptr(new Connection(ios, bufferSize, timeoutSecs));
	}
	void start() {
		// start reader/writer
		std::cerr << __FILE__ << "(" << __LINE__ << ") Connection::start()" << std::endl;
		read();
	}
	boost::asio::ip::tcp::socket& socket() {
		return socket_;
	}
private:
	void echo() {
		memcpy(writeBuffer_, readBuffer_, bytesRead);
		bytesToWrite = bytesRead;
		bytesRead = 0;

#ifdef PRINT_BUFFER
				print_buffer(writeBuffer_, bytesToWrite, "[ECHO] ");
#endif

		write();
	}
	void read() {
		socket_.async_read_some(
				boost::asio::buffer(readBuffer_, BUFFER_SIZE),
				boost::bind(
						&Connection::onReadComplete,
						shared_from_this(),
						_1,
						_2
						)
		);
	}
	void write() {
		async_write(
				socket_,
				boost::asio::buffer(writeBuffer_, bytesToWrite),
				boost::bind(
						&Connection::onWriteComplete,
						shared_from_this(),
						_1,
						_2
						)
		);
	}
	void onReadComplete(
			const boost::system::error_code& ec,
			size_t bytes_transferred
		) {
		if(!ec) {
			bytesRead += bytes_transferred;
			if(bytesRead != 0) {
				echo();
			}
		} else {
			std::cerr << __FILE__ << "(" << __LINE__ << ") " << ec << std::endl;
		}

	}
	void onWriteComplete(
			const boost::system::error_code& ec,
			size_t bytes_transferred
		) {
		if(!ec) {
			read();
		} else {
			std::cerr << __FILE__ << "(" << __LINE__ << ") " << ec << std::endl;
		}
	}
	boost::asio::io_service& ios_;
	boost::asio::ip::tcp::socket socket_;
	const std::size_t BUFFER_SIZE;
	char* const readBuffer_;
	char* const writeBuffer_;
	const std::size_t TIMEOUT_SECONDS;
	std::size_t bytesToWrite;
	std::size_t bytesRead;
	LogFilter::Ptr logFilter_;
	CodecFactory::Ptr codecFactory_;
};

class Acceptor : public boost::enable_shared_from_this<Acceptor> {
public:
	typedef boost::shared_ptr<Acceptor> Ptr;
	Acceptor(boost::asio::io_service& ios,
			int port,
			std::size_t bufferSize,
			std::size_t timeoutSecs)
		:ios_(ios),
		port_(port),
		acceptor_(ios, boost::asio::ip::tcp::endpoint(boost::asio::ip::tcp::v4(), port)),
		BUFFER_SIZE(bufferSize),
		TIMEOUT_SECONDS(timeoutSecs) {
	}
	virtual ~Acceptor() { }
	void start() {
		accept();
	}
private:
	void onAccept(Connection::Ptr conn, const boost::system::error_code& ec) {
		std::cerr << "Acceptor::onAccept(): " << ec << std::endl;
		if(!ec) {
			conn->start();
		} else {
			// accept failed.
		}
		accept();
	}
	void accept() {
		std::cerr << "Acceptor::accept()" << std::endl;
		Connection::Ptr ptr = Connection::newConnection(ios_, BUFFER_SIZE, TIMEOUT_SECONDS);
		acceptor_.async_accept(ptr->socket(),
				boost::bind(&Acceptor::onAccept,
						shared_from_this(),
						ptr, boost::asio::placeholders::error)
			);
	}
	boost::asio::io_service& ios_;
	const int port_;
	boost::asio::ip::tcp::acceptor acceptor_;
	const std::size_t BUFFER_SIZE;
	const std::size_t TIMEOUT_SECONDS;
};

} /* namespace nw */

#endif /* INCLUDE_NW_ACCEPTOR_HPP_ */
