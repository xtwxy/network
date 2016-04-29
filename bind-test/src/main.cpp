#include <cstdlib>
#include <iostream>
#include <cstdio>
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/system/error_code.hpp>

typedef boost::function<
  void (boost::system::error_code&,
        char *,
        std::size_t)
  > Handler;

void handleMessage(
    boost::system::error_code& ec,
    char* msg,
    std::size_t msg_size) {
  if(!ec) {
    for(std::size_t i = 0; i != msg_size; ++i) {
      printf("%2.2x ", 0xff & msg[i]);
    }
    printf("\n");
  } else {
    std::cout << ec << std::endl;
  }
}

int main(int argc, char* argv[]) {

  char buff[] {1,2,3,4,5,6,7,8,9,0};

  Handler handle = handleMessage;

  boost::system::error_code ec = 
      boost::system::errc::make_error_code(
          boost::system::errc::success);

  handle(ec, buff, sizeof(buff));

  return EXIT_SUCCESS;
}
