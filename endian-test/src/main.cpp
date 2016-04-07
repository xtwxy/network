#include <iostream>
#include <cstdio>
#include <boost/endian/buffers.hpp>  // see Synopsis below
#include <boost/static_assert.hpp>

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

int main(int, char* [])
{
  header h;

  BOOST_STATIC_ASSERT(sizeof(h) == 16U);  // reality check

  h.file_code   = 0x01020304;
  h.file_length = sizeof(header);
  h.version     = 1;
  h.shape_type  = 0x01020304;

  //  Low-level I/O such as POSIX read/write or <cstdio>
  //  fread/fwrite is sometimes used for binary file operations
  //  when ultimate efficiency is important. Such I/O is often
  //  performed in some C++ wrapper class, but to drive home the
  //  point that endian integers are often used in fairly
  //  low-level code that does bulk I/O operations, <cstdio>
  //  fopen/fwrite is used for I/O in this example.

  std::FILE* fi = std::fopen(filename, "wb");  // MUST BE BINARY

  if (!fi)
  {
    std::cout << "could not open " << filename << '\n';
    return 1;
  }

  if (std::fwrite(&h, sizeof(header), 1, fi)!= 1)
  {
    std::cout << "write failure for " << filename << '\n';
    return 1;
  }

  std::fclose(fi);

  std::cout << "created file " << filename << '\n';

  return 0;
}
