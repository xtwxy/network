#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <vector>
#include <boost/any.hpp>

int main(int argc, char* argv[]) {

  std::vector<char> buff = {1,2,3,4,5,6,7,8,9,0};

  boost::any data(buff);

  std::cout << "data.type().name() == " << data.type().name() << std::endl;
  std::cout << "typeid(std::vector<char>).name() == " << typeid(std::vector<char>).name() << std::endl;
  std::cout << "(data.type() == typeid(std::vector<char>)) == " << (data.type() == typeid(std::vector<char>)) << std::endl;

  return EXIT_SUCCESS;
}
