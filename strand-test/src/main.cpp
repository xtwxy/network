#include <iostream>
#include <cstdio>
#include <boost/asio.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>

using namespace boost::asio;

void handler(int i, boost::function<void (int)> f) {
  std::cout << "handler(" << i << ") called." << std::endl;
  f(i);
}
void complete(int i) {
  std::cout << "complete(" << i << ") called." << std::endl;
}
io_service _ios;
io_service::strand _strand(_ios);

int main(int, char* [])
{
  for(int i = 0; i < 10; ++i) {
    _ios.post(boost::bind(handler, i,
                         _strand.wrap(complete)
                         )
             );
  }
  _ios.run();

  return 0;
}
