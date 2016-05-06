/*
 * DataHandler.cpp
 *
 *  Created on: May 6, 2016
 *      Author: master
 */

#include <iostream>
#include <iterator>
#include <algorithm>
#include <boost/bind.hpp>
#include <boost/make_shared.hpp>

#include "DataHandler.h"

using namespace std;
using namespace codec;

namespace dc2800_codec {

DataHandler::DataHandler() {

}

DataHandler::~DataHandler() {

}

void DataHandler::handle(codec::Context& ctx,	boost::any& data) {
//	BufferPtr psb = boost::any_cast<BufferPtr>(data);
//	std::size_t len = psb->size();
//
//	BufferPtr buff = boost::make_shared<boost::asio::streambuf>();
//
//	istream input(psb.get());
//	ostream output(buff.get());
//
//	copy(istream_iterator<char>(input),
//			istream_iterator<char>(),
//			ostream_iterator<char>(output));
//
//	boost::any out = buff;
//
//	ctx.write(out);

	BufferPtr psb = boost::any_cast<BufferPtr>(data);
	std::size_t len = psb->size();
	if(len == 0) return;
	BufferPtr buff = boost::make_shared<boost::asio::streambuf>();
	boost::asio::streambuf::mutable_buffers_type mbs = buff->prepare(len);
	boost::asio::mutable_buffer mb = *(mbs.begin());
	std::size_t mbsize = boost::asio::detail::buffer_size_helper(mb);
	char* mbdata = reinterpret_cast<char*>(boost::asio::detail::buffer_cast_helper(mb));

	std::size_t index = 0;
	boost::asio::streambuf::const_buffers_type cbs = psb->data();
	for(auto it = cbs.begin(); it != cbs.end(); ++it) {
		boost::asio::const_buffer cb = *(it);
		std::size_t cbsize = boost::asio::detail::buffer_size_helper(cb);
		const char* cbdata = reinterpret_cast<const char*>(boost::asio::detail::buffer_cast_helper(cb));
		std::size_t icb = 0;
		for(; icb != cbsize; ++icb) {
			mbdata[index + icb] = cbdata[icb];
		}
		index += icb;
	}
	assert(index == len);
	buff->commit(len);
	psb->consume(len);

	boost::any out = buff;

	ctx.write(out);
}

void DataHandler::sessionStart(codec::Context& ctx) {

}

void DataHandler::sessionClose(codec::Context& ctx) {

}

void DataHandler::exceptionCaught(codec::Context& ctx, const std::exception& ex) {

}

HandlerPtr DataHandler::getHandler() {
	HandlerFunc handler = boost::bind(&DataHandler::handle, shared_from_this(), _1, _2);
	SessionStart sessionStart = boost::bind(&DataHandler::sessionStart, shared_from_this(), _1);
	SessionClose sessionClose = boost::bind(&DataHandler::sessionClose, shared_from_this(), _1);
	ExceptionCaught exceptionCaught = boost::bind(&DataHandler::exceptionCaught, shared_from_this(), _1, _2);
	HandlerPtr ptr(new Handler(
				handler,
				sessionStart,
				sessionClose,
				exceptionCaught
			));
	return ptr;
}

} /* namespace dc2800_codec */
