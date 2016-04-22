/*
 * Command.cpp
 *
 *  Created on: Apr 19, 2016
 *      Author: master
 */

#include <boost/bind.hpp>
#include "Command.h"

namespace codec {

Command::Command(SessionPtr session,
                 FrameEncoder frameEncoder,
                 DataEncoder dataEncoder,
                 FrameDecoder frameDecoder,
                 DataDecoder dataDecoder,
                 CompletionHandler handler)
  : session(session),
    encodeFrame(frameEncoder),
    encodeData(dataEncoder),
    decodeFrame(frameDecoder),
    decodeData(dataDecoder),
    complete(handler) {
    }

Command::~Command() {
}

void Command::execute() {
}

} /* namespace battery */
