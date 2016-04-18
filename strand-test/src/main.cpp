#include <iostream>
#include <cstdio>
#include <boost/asio.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/thread/thread.hpp>

const int THREAD_COUNT = 8;

using namespace boost::asio;

io_service _ios;
io_service::strand _strand(_ios);

void handler(int i, boost::function<void (int)> f) {
  std::cout << "handler(" << i << ") called." << std::endl;
  _ios.post(
      _strand.wrap(
      boost::bind(f, i)
      )
      );
}
void complete(int i) {
  std::cout << "complete(" << i << ") called." << std::endl;
}

int main(int, char* [])
{

  for(int i = 0; i < THREAD_COUNT*2048; ++i) {
    _ios.post(boost::bind(handler, i,
                         _strand.wrap(
                             complete
                             )
                         )
             );
  }
  _ios.run();

  return 0;
}
