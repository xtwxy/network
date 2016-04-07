#include <boost/test/unit_test.hpp>
#include <Codecable.hpp>

template <typename T>
class MyValidator : public Validator {
public:
 MyValidator(T t) : t_(t) { }
 virtual ~MyValidator() { }

 virtual bool validate(boost::any value) {
   if(value.type() == typeid(T)) {
     return (boost::any_cast<T>(value) == t_);
   }
   return false;
 }
private:
 T t_;
};

BOOST_AUTO_TEST_SUITE( ValidatorTest )

BOOST_AUTO_TEST_CASE( test_validate ) {

  Validator::Ptr v(new MyValidator<int>(10));

  BOOST_CHECK_EQUAL(v->validate(boost::any(10)), 1);

}

BOOST_AUTO_TEST_SUITE_END()

