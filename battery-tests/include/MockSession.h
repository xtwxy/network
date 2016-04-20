/*
 * MockSession.h
 *
 *  Created on: Apr 20, 2016
 *      Author: master
 */

#ifndef SRC_MOCKSESSION_H_
#define SRC_MOCKSESSION_H_

#include "Command.h"

class MockSession {
public:
	MockSession(char* rdbuf, std::size_t availableToRead,
			char* wrbuf, std::size_t wrbufSize);
	virtual ~MockSession();

	void read(char*, std::size_t, battery::Session::Handler);
	void write(char*, std::size_t, battery::Session::Handler);
	void readComplete(const boost::system::error_code&, std::size_t);
	void writeComplete(const boost::system::error_code&, std::size_t);

private:
	char* const readBuffer_;
	std::size_t availableToRead_;
	char* const writeBuffer_;
	std::size_t writeBufferSize_;
	std::size_t bytesRead_;
	std::size_t bytesWritten_;
};

#endif /* SRC_MOCKSESSION_H_ */
