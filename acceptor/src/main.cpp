#include <iostream>
#include <map>
#include <string>
#include <boost/asio.hpp>
#include <boost/asio/buffer.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

#include <boost/date_time/posix_time/posix_time.hpp>

class Connection : public boost::enable_shared_from_this<Connection> {
public:
	typedef boost::shared_ptr<Connection> Ptr;
	Connection(boost::asio::io_service& ios, std::size_t timeoutSecs) 
		:ios_(ios), socket_(ios), TIMEOUT_SECONDS(timeoutSecs) { }
	virtual ~Connection() { }
	
	static Ptr newConnection(boost::asio::io_service& ios, std::size_t timeoutSecs) {
		return Ptr(new Connection(ios, timeoutSecs));
	}
	void start() { 
	}
	boost::asio::ip::tcp::socket& socket() {
		return socket_;
	}
private:
	boost::asio::io_service& ios_;
	boost::asio::ip::tcp::socket socket_;
	const std::size_t TIMEOUT_SECONDS;
};

class Acceptor : public boost::enable_shared_from_this<Acceptor> {
public:
	typedef boost::shared_ptr<Acceptor> Ptr;
	Acceptor(boost::asio::io_service& ios, int port, std::size_t timeoutSecs) 
		:ios_(ios), 
		port_(port),
		acceptor_(ios, boost::asio::ip::tcp::endpoint(boost::asio::ip::tcp::v4(), 2001)), 
		TIMEOUT_SECONDS(timeoutSecs) {
	}
	virtual ~Acceptor() { }
	void start() {
		accept();
	}
private:
	void onAccept(Connection::Ptr conn, boost::system::error_code& ec) {
		if(!ec) {
			conn->start();
		} else {
			// accept failed.
		}
		accept();
	}
	void accept() {
		Connection::Ptr ptr = Connection::newConnection(ios_, TIMEOUT_SECONDS);
		acceptor_.async_accept(ptr->socket(), 
				boost::bind(&Acceptor::onAccept, shared_from_this(), ptr, _1)
			);
	}
	boost::asio::io_service& ios_;
	const int port_;
	boost::asio::ip::tcp::acceptor acceptor_;
	const std::size_t TIMEOUT_SECONDS;
};

int main(int argc, char* argv[]) {
	boost::asio::io_service ios;
	
	ios.run();
	return EXIT_SUCCESS;
}

