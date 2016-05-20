#include <fstream>
#include <iostream>
#include <cstdio>
#include <boost/endian/buffers.hpp>  // see Synopsis below
#include <boost/static_assert.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>

using namespace boost::endian;

namespace
{
  //  This is an extract from a very widely used GIS file format.
  //  Why the designer decided to mix big and little endians in
  //  the same file is not known. But this is a real-world format
  //  and users wishing to write low level code manipulating these
  //  files have to deal with the mixed endianness.

  struct header
  {
    big_int32_buf_t     file_code;
    big_int32_buf_t     file_length;
    little_int32_buf_t  version;
    little_int32_buf_t  shape_type;
  };

  const char* filename = "test.dat";
}

namespace boost {
namespace serialization {

template<class Archive>
void serialize(Archive & ar, header & g, const unsigned int version)
{
	const char* cp = reinterpret_cast<const char*>(&g);
	for(std::size_t i = 0; i != sizeof(g); ++i) {
		ar & *(cp + i);
	}
}

} // namespace serialization
} // namespace boost


int main(int, char* [])
{
  header h;

  BOOST_STATIC_ASSERT(sizeof(h) == 16U);  // reality check

  h.file_code   = 0xcafebabe;
  h.file_length = sizeof(header);
  h.version     = 0xcadecade;
  h.shape_type  = 0xdeadfeed;

  std::ofstream ofs("test.dat");
  boost::archive::binary_oarchive oa(ofs);
  oa << h;

  return 0;
}
