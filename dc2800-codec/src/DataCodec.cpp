/*
 * DataCodec.cpp
 *
 *  Created on: May 6, 2016
 *      Author: master
 */

#include <algorithm>
#include <boost/bind.hpp>
#include <boost/make_shared.hpp>

#include "Protocol.h"
#include "DataCodec.h"

using namespace codec;
using namespace std;

namespace dc2800_codec {

DataCodec::DataCodec() {
}

DataCodec::~DataCodec() {
}

codec::CodecPtr DataCodec::getCodec() {
	Ptr codec = boost::make_shared<DataCodec>();
	EncodeFunc encoder = boost::bind(&DataCodec::encode, codec,	_1, _2,	_3);
	EncodeFunc decoder = boost::bind(&DataCodec::decode, codec,	_1, _2,	_3);

	SessionStart ssnStart = boost::bind(&DataCodec::sessionStart, codec, _1);
	SessionClose ssnClose = boost::bind(&DataCodec::sessionClose, codec, _1);

	ExceptionCaught exceptCaught = boost::bind(&DataCodec::exceptionCaught, codec, _1, _2);

	CodecPtr ptr(new Codec(encoder, decoder, ssnStart, ssnClose, exceptCaught));

	return ptr;

}

void DataCodec::encode(codec::Context& ctx, boost::any& input, std::list<boost::any>& output) {
	output.push_back(input);
}

void DataCodec::decode(codec::Context& ctx, boost::any& input, std::list<boost::any>& output) {
	output.push_back(input);
}

void DataCodec::sessionStart(codec::Context& ctx) {

}

void DataCodec::sessionClose(codec::Context& ctx) {

}

void DataCodec::exceptionCaught(codec::Context& ctx, const std::exception& ex) {

}

} /* namespace dc2800_codec */
