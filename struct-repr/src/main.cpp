#include <iostream>
#include <iomanip>

template<std::size_t N>
struct non_virtual_bytes {
  char bytes[N];
};

template<std::size_t N>
struct non_virtual_child : public non_virtual_bytes<N> {
};

template<std::size_t N>
struct virtual_destructor_bytes {
  virtual ~virtual_destructor_bytes() { }
  char bytes[N];
};

template<std::size_t N>
struct virtual_member_bytes {
  virtual char* get_bytes() { return bytes; }
  char bytes[N];
};

template<std::size_t N>
struct virtual_des_mem_bytes {
  virtual ~virtual_des_mem_bytes() { }
  virtual char* get_bytes() { return bytes; }
  char bytes[N];
};

template<std::size_t N>
struct virtual_destructor_child : public virtual_destructor_bytes<N> {
  virtual ~virtual_destructor_child() { }
};

template<std::size_t N>
struct virtual_member_child : public virtual_member_bytes<N> {
  virtual ~virtual_member_child() { }
};

template<std::size_t N>
struct  virtual_des_mem_child : virtual_des_mem_bytes<N> {
  virtual ~virtual_des_mem_child() { }
};

#define DECLARE_PRINT_STRUCT(T)                   \
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

DECLARE_PRINT_STRUCT(non_virtual_bytes)
DECLARE_PRINT_STRUCT(non_virtual_child)
DECLARE_PRINT_STRUCT(virtual_destructor_bytes)
DECLARE_PRINT_STRUCT(virtual_member_bytes)
DECLARE_PRINT_STRUCT(virtual_des_mem_bytes)
DECLARE_PRINT_STRUCT(virtual_destructor_child)
DECLARE_PRINT_STRUCT(virtual_member_child)
DECLARE_PRINT_STRUCT(virtual_des_mem_child)

int main(int argc, char* argv[]) {
  repeat_non_virtual_bytes<8>();
  repeat_non_virtual_child<8>();
  repeat_virtual_destructor_bytes<8>();
  repeat_virtual_member_bytes<8>();
  repeat_virtual_des_mem_bytes<8>();
  repeat_virtual_destructor_child<8>();
  repeat_virtual_member_child<8>();
  repeat_virtual_des_mem_child<8>();
 
  non_virtual_bytes<8>* nvbp = new non_virtual_child<8>();
  // The following statement cannot compile
  // because both the parent and the child, has no virtual
  // member function.
  // non_virtual_child<8>* nvcp = dynamic_cast<non_virtual_child<8>*>(nvbp);
  delete nvbp;

  virtual_destructor_bytes<8>* vdbp = new virtual_destructor_child<8>();
  virtual_destructor_child<8>* vdcp = dynamic_cast<virtual_destructor_child<8>*>(vdbp);
  delete vdbp;

  return EXIT_SUCCESS;
}
