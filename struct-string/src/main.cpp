#include <iostream>
#include <iomanip>

struct CS {
  CS(const std::string s) : length(s.length()) {
  }
  const std::size_t length;
  char value[length];
};
int main(int argc, char* argv[]) {
  std::cout << sizeof(CS) << std::endl;

  return EXIT_SUCCESS;
}
