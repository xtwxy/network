#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <vector>
#include <array>
#include <algorithm>
#include <boost/any.hpp>

class A {
public:
	A() { }
	~A() { }
};

class B : public A {
public:
	B() { }
	~B() { }
};

int main(int argc, char* argv[]) {

  std::vector<char> buff = {1,2,3,4,5,6,7,8,9,0};
  std::array<char, 16> array;
  boost::any any_array(array);
  std::cout << "any_array.type().name() == " << any_array.type().name() << std::endl;

  auto any = boost::any_cast<std::array<char, 16>& >(any_array); 
  for(auto it = any.begin(); it != any.end(); ++it) {
    printf("%2.2x ", 0xff & (*it));
  }
  printf("\n");

  boost::any data(buff);

  std::cout << "data.type().name() == " << data.type().name() << std::endl;
  std::cout << "typeid(std::vector<char>).name() == " << typeid(std::vector<char>).name() << std::endl;
  std::cout << "(data.type() == typeid(std::vector<char>)) == " << (data.type() == typeid(std::vector<char>)) << std::endl;

  boost::any ldata = data;

  std::cout << "data.type().name() == " << data.type().name() << std::endl;
  std::cout << "typeid(std::vector<char>).name() == " << typeid(std::vector<char>).name() << std::endl;
  std::cout << "(data.type() == typeid(std::vector<char>)) == " << (data.type() == typeid(std::vector<char>)) << std::endl;
  std::cout << "boost::any_cast<std::vector<char> >(data).size() = " << boost::any_cast<std::vector<char> >(data).size() << std::endl;

  std::cout << "ldata.type().name() == " << ldata.type().name() << std::endl;
  std::cout << "typeid(std::vector<char>).name() == " << typeid(std::vector<char>).name() << std::endl;
  std::cout << "(ldata.type() == typeid(std::vector<char>)) == " << (ldata.type() == typeid(std::vector<char>)) << std::endl;
  std::cout << "boost::any_cast<std::vector<char> >(ldata).size() = " << boost::any_cast<std::vector<char> >(ldata).size() << std::endl;

  B b;
  boost::any anyB = b;
  boost::any anyA = boost::any_cast<A>(anyB);


  return EXIT_SUCCESS;
}
