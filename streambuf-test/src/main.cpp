#include <cstdlib>
#include <iostream>
#include <map>
#include <string>
#include <boost/asio.hpp>
#include <boost/asio/buffer.hpp>

using namespace std;
using namespace boost;
using namespace boost::asio;

static size_t BUFFER_SIZE = 8;
boost::asio::streambuf buff;


void test() {  
  boost::asio::streambuf::mutable_buffers_type bufs 
      = buff.prepare(BUFFER_SIZE);
  boost::asio::streambuf::mutable_buffers_type::const_iterator it
      = bufs.begin();

  for(; it != bufs.end(); ++it) {
    mutable_buffer b = *it;

    std::size_t size = boost::asio::detail::buffer_size_helper(b);
    char* data = reinterpret_cast<char*>(boost::asio::detail::buffer_cast_helper(b));

    for(std::size_t i = 0; i != size; ++i) {
      data[i] = i;
    }
    buff.commit(size);
    break;
  }

  cout << "size = " << buff.size() << endl;

  boost::asio::streambuf::const_buffers_type cbufs
      = buff.data();
  boost::asio::streambuf::const_buffers_type::const_iterator cit
      = cbufs.begin();

  for(; cit != cbufs.end(); ++cit) {
    const_buffer b = *cit;

    std::size_t size = boost::asio::detail::buffer_size_helper(b);
    const char* data = reinterpret_cast<const char*>(boost::asio::detail::buffer_cast_helper(b));

    for(std::size_t i = 0; i != size; ++i) {
      printf("%2.2x ", 0xff & i);
    }
    printf("\n");
    buff.consume(size);
  }
  cout << "size = " << buff.size() << endl;
}

int main(int argc, char* argv[]) {
  for(std::size_t i = 0; i < 1000000000; ++i) {
    test();
  }
  return EXIT_SUCCESS;
}

