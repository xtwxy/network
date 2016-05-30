#include <cstdlib>
#include <iostream>

template<std::size_t N>
struct non_virtual_bytes {
  char bytes[N];
};

#define PRINT_STRUCT(T)                           \
template<std::size_t N>                           \
std::size_t repeat_##T() {                        \
  std::size_t size = repeat_##T<N - 1>();         \
  std::cout << "The size of "                     \
            << #T << "<" << N - 1 << ">: ";       \
  std::cout << size << std::endl;                 \
  return sizeof(T<N>);                            \
}                                                 \
template<>                                        \
std::size_t repeat_##T<0>() {                     \
  return sizeof(T<0>);                            \
}

PRINT_STRUCT(non_virtual_bytes)

int main(int argc, char* argv[]) {
  repeat_non_virtual_bytes<8>();

  return EXIT_SUCCESS;
}
