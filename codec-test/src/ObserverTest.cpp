#include <boost/test/unit_test.hpp>
#include <Codecable.hpp>

template <typename T>
class MyObserver : public Observer {
public:
 typedef boost::shared_ptr<MyObserver<T> > Ptr;
 MyObserver(T t) : t_(t) { }
 virtual ~MyObserver() { }

 virtual bool update(boost::any value) {
   if(value.type() == typeid(T)) {
     t_ = boost::any_cast<T>(value);
     return true;
   }
   return false;
 }
 T value() {
   return t_;
 }
private:
 T t_;
};

BOOST_AUTO_TEST_SUITE( ObserverTest )

BOOST_AUTO_TEST_CASE( test_validate ) {
  
  const int V = 10;
  
  MyObserver<int>::Ptr my(new MyObserver<int>(V));
  Observer::Ptr v = my;

  BOOST_CHECK_EQUAL(v->update(boost::any(V)), true);
  BOOST_CHECK_EQUAL(V, my->value());

}

BOOST_AUTO_TEST_SUITE_END()

