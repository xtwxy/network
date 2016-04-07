#include <boost/test/unit_test.hpp>
#include <Codecable.hpp>

template <typename T>
class MyInitializer : public Initializer {
public:
 MyInitializer(T t) : t_(t) { }
 virtual ~MyInitializer() { }

 virtual boost::any getValue() {
  return t_; 
 }
private:
 boost::any t_;
};

BOOST_AUTO_TEST_SUITE( InitializerTest )

BOOST_AUTO_TEST_CASE( test_validate ) {

  Initializer::Ptr v(new MyInitializer<int>(10));
  
  BOOST_CHECK(v->getValue().type() == typeid(int));
  
  int i = boost::any_cast<int>(v->getValue());

  BOOST_CHECK_EQUAL(i, 10);

}

BOOST_AUTO_TEST_SUITE_END()

