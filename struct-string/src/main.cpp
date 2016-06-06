#include <iostream>
#include <iomanip>
#include <boost/date_time/local_time/local_time.hpp>

enum SignalType { AI=1, DI=2, SI=3, AO=4, DO=8, SO=12 };

template<std::size_t N>
struct CS {
  CS(const std::string s) : length(N) {
  }
  const std::size_t length;
  char value[N];
};

int main(int argc, char* argv[]) {
  std::cout << sizeof(CS<10>) << std::endl;
  std::cout << sizeof(SignalType) << std::endl;
  std::cout << sizeof(boost::posix_time::ptime) << std::endl;

  boost::posix_time::ptime ptime = boost::posix_time::second_clock::local_time();
  tm t = to_tm(ptime);
  std::cout << sizeof(t) << std::endl;
  std::cout << "sizeof(time_t) = " << sizeof(time_t) << std::endl;

  return EXIT_SUCCESS;
}
