#include <boost/test/unit_test.hpp>
#include <Codecable.hpp>

template <typename T>
class MyCodecable : public Codecable {
public:
 MyCodecable(T t) : t_(t) { }
 virtual ~MyCodecable() { }

 virtual std::size_t encode(char* bytes, std::size_t offset, int length,
                            bool& completed) {
  return 0;
 }
 virtual std::size_t decode(char* bytes, std::size_t offset, int length,
                            bool& completed) {
  return 0;
 }
private:
 T t_;
};

BOOST_AUTO_TEST_SUITE( CodecableTest )

BOOST_AUTO_TEST_CASE( test_validate ) {

  char buff[sizeof(int)];

  Codecable::Ptr v(new MyCodecable<int>(10));

  //BOOST_CHECK_EQUAL(v->validate(boost::any(10)), 1);

}

BOOST_AUTO_TEST_SUITE_END()

