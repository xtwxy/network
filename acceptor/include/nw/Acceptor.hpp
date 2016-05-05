/*
 * Acceptor.h
 *
 *  Created on: Mar 22, 2016
 *      Author: master
 */

#ifndef INCLUDE_NW_ACCEPTOR_HPP_
#define INCLUDE_NW_ACCEPTOR_HPP_

#include <cstdio>
#include <iostream>
#include <boost/asio.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

#include "Codec.h"

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

	Connection(boost::asio::io_service& ios)
		:ios_(ios),
		 socket_(ios),
		 pipeline_(new codec::Pipeline(ios)) {

	}

	virtual ~Connection() {
		std::cerr << "Connection::~Connection()" << std::endl;
		close();
	}

	static Ptr newConnection(boost::asio::io_service& ios) {
		return Ptr(new Connection(ios));
	}

	void start(codec::PipelineInitializer initialize) {
		pipeline_->setSession(getSession());
		initialize(*pipeline_);
		pipeline_->start();
	}

	boost::asio::ip::tcp::socket& socket() {
		return socket_;
	}

private:
	void read(codec::Session::ReadBuffer buff,
			codec::Session::IoCompHandler handler) {
		socket_.async_read_some(buff, handler);
	}

	void write(codec::Session::WriteBuffer buff,
			codec::Session::IoCompHandler handler) {
		async_write(socket_, buff, handler);
	}

	void close() {
		try {
			socket_.shutdown(boost::asio::ip::tcp::socket::shutdown_both);
		} catch(...) { }
		try { socket_.close(); } catch(...) { }
	}

	void post(codec::Session::Task t) {
		ios_.post(t);
	}

	codec::SessionPtr getSession() {
		codec::SessionPtr ptr(
				new codec::Session(
						boost::bind(&Connection::read, shared_from_this(), _1, _2),
						boost::bind(&Connection::write, shared_from_this(), _1, _2),
						boost::bind(&Connection::post, shared_from_this(), _1),
						boost::bind(&Connection::close, shared_from_this())));
		return ptr;
	}

	boost::asio::io_service& ios_;
	boost::asio::ip::tcp::socket socket_;
	codec::PipelinePtr pipeline_;
};

class Acceptor : public boost::enable_shared_from_this<Acceptor> {
public:
	typedef boost::shared_ptr<Acceptor> Ptr;
	Acceptor(boost::asio::io_service& ios)
		:ios_(ios),
		acceptor_(ios),
		port_(1999) {
	}
	virtual ~Acceptor() { }

	Acceptor& setPort(unsigned short port) {
		port_ = port;
		boost::asio::ip::tcp::endpoint endpoint(boost::asio::ip::tcp::v4(), port);
		acceptor_.open(endpoint.protocol());
		acceptor_.set_option(boost::asio::ip::tcp::acceptor::reuse_address(true));
		acceptor_.bind(endpoint);
		acceptor_.listen();
		return *this;
	}

	Acceptor& setPipelineInitializer(codec::PipelineInitializer initializer) {
		initializer_ = initializer;
		return *this;
	}

	void start() {
		accept();
	}
private:
	void onAccept(Connection::Ptr conn, const boost::system::error_code& ec) {
		std::cerr << "Acceptor::onAccept(): " << ec << std::endl;
		if(!ec) {
			conn->start(initializer_);
		} else {
			// accept failed.
		}
		accept();
	}
	void accept() {
		std::cerr << "Acceptor::accept()" << std::endl;
		Connection::Ptr ptr = Connection::newConnection(ios_);
		acceptor_.async_accept(ptr->socket(),
				boost::bind(&Acceptor::onAccept,
						shared_from_this(),
						ptr, boost::asio::placeholders::error)
			);
	}
	boost::asio::io_service& ios_;
	unsigned short port_;
	boost::asio::ip::tcp::acceptor acceptor_;
	codec::PipelineInitializer initializer_;
};

} /* namespace nw */

#endif /* INCLUDE_NW_ACCEPTOR_HPP_ */
