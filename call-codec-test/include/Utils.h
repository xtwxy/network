/*
 * Utils.h
 *
 *  Created on: May 30, 2016
 *      Author: master
 */

#ifndef INCLUDE_UTILS_H_
#define INCLUDE_UTILS_H_

#include <iostream>

#ifdef NDEBUG
#define PRINT_MESSAGE(message)
#else
#define PRINT_MESSAGE(message) {                                                             \
const unsigned char* p = reinterpret_cast<const unsigned char *>(&message);                  \
std::cout << #message << ": " << typeid(message).name() << endl;                             \
for(size_t i = 0; i != sizeof(message); ++i) {                                               \
	std::cout << "0x"<< std::hex << std::setw(2) << std::setfill('0')                        \
		<< static_cast<unsigned short>(*(p + i)) << ", ";                                    \
}                                                                                            \
std::cout << endl;                                                                           \
}
#endif



#endif /* INCLUDE_UTILS_H_ */
