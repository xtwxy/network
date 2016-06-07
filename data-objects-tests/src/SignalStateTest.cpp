#include <boost/test/unit_test.hpp>
#include <boost/make_shared.hpp>
#include <boost/asio/streambuf.hpp>

#include <iostream>
#include <iterator>
#include <iomanip>
#include <ctime>
#include <cmath>
#include <limits>

#include "SignalState.h"

using namespace std;
using namespace boost;
using namespace DataObjects;

BOOST_AUTO_TEST_SUITE( SignalStateTest )

bool print_streambuf(boost::asio::streambuf& sb) {
  std::size_t size = sb.size();
  uint8_t buff[size];
  sb.sgetn(reinterpret_cast<char*>(buff), sizeof(buff));
  for(std::size_t i = 0; i != size; ++i) {
	  std::cout << "0x"<< std::hex << std::setw(2) << std::setfill('0')
		  << (0xff& buff[i]) << ", ";
  }
  std::cout << std::endl;
  return true;
}

class MyBooleanListener : public StateListener {
 public:
  typedef boost::shared_ptr<MyBooleanListener> Ptr;

  MyBooleanListener() : stateChangeCalled(false) { }
  virtual ~MyBooleanListener() { }

  void stateChanged(StateEventPtr se) {
    stateChangeCalled = true;

    uint8_t expected[] = {
      0x02, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00,
      0x02, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x01
    };

    asio::streambuf sb;
    se->store(sb);
    //print_streambuf(sb);

    std::size_t length =  sb.size();
    BOOST_CHECK_EQUAL(length, sizeof(expected));

    uint8_t buff[length];
    sb.sgetn(reinterpret_cast<char*>(buff), sizeof(buff));

    for(std::size_t i = 0; i != length; ++i) {
      if(i > 0 && i < 9) continue;
      BOOST_CHECK_EQUAL(buff[i], expected[i]);
    }
  }

  bool isStateChanged() const {
    return stateChangeCalled;
  }
 private:
  bool stateChangeCalled;
};

BOOST_AUTO_TEST_CASE( testSaveSignalId ) {
  uint8_t expected[] = {
    0x17, 0x00, 0x00, 0x00, 0x32,
    0x30, 0x31, 0x36, 0x2d, 0x30,
    0x36, 0x2d, 0x30, 0x36, 0x20,
    0x30, 0x30, 0x3a, 0x30, 0x30,
    0x3a, 0x30, 0x30, 0x2e, 0x30,
    0x30, 0x30
  };
  asio::streambuf sb;
  SignalId id("2016-06-06 00:00:00.000");
  id.store(sb);

  std::size_t length = sb.size();
  BOOST_CHECK_EQUAL(length, sizeof(expected));

  uint8_t buff[length];
  sb.sgetn(reinterpret_cast<char*>(buff), sizeof(buff));

  for(std::size_t i = 0; i != length; ++i) {
    BOOST_CHECK_EQUAL(buff[i], expected[i]);
  }
}

BOOST_AUTO_TEST_CASE( testLoadSignalId ) {
  uint8_t expected[] = {
    0x17, 0x00, 0x00, 0x00, 0x32,
    0x30, 0x31, 0x36, 0x2d, 0x30,
    0x36, 0x2d, 0x30, 0x36, 0x20,
    0x30, 0x30, 0x3a, 0x30, 0x30,
    0x3a, 0x30, 0x30, 0x2e, 0x30,
    0x30, 0x30
  };
  asio::streambuf sb;
  sb.sputn(reinterpret_cast<char*>(&expected), sizeof(expected));

  SignalId id;
  id.load(sb);

  BOOST_CHECK_EQUAL(id.getValue(), "2016-06-06 00:00:00.000");
}

BOOST_AUTO_TEST_CASE( testSaveBooleanState ) {
  uint8_t expected[] = {
    0x02, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x01
  };
  BooleanState bs;
  bs.setValue(true);

  asio::streambuf sb;
  bs.store(sb);

  std::size_t length =  sb.size();
  BOOST_CHECK_EQUAL(length, sizeof(expected));

  uint8_t buff[length];
  sb.sgetn(reinterpret_cast<char*>(buff), sizeof(buff));

  for(std::size_t i = 0; i != length; ++i) {
    if(i > 0 && i < 9) continue;
    BOOST_CHECK_EQUAL(buff[i], expected[i]);
  }
}

BOOST_AUTO_TEST_CASE( testLoadBooleanState ) {
  uint8_t expected[] = {
    0x02, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x01
  };

  asio::streambuf sb;
  sb.sputn(reinterpret_cast<char*>(&expected), sizeof(expected));

  BooleanState bs;
  bs.load(sb);

  BOOST_CHECK_EQUAL(bs.getValue(), true);
}

BOOST_AUTO_TEST_CASE( testSaveAnalogState ) {
  uint8_t expected[] = {
    0x01, 0xd1, 0x44, 0x55, 0x57,
    0x00, 0x00, 0x00, 0x00, 0x90,
    0xf7, 0xaa, 0x95, 0x09, 0xbf,
    0x05, 0x40
  };
  AnalogState as;
  as.setValue(2.71828);

  asio::streambuf sb;
  as.store(sb);

  std::size_t length =  sb.size();
  BOOST_CHECK_EQUAL(length, sizeof(expected));

  uint8_t buff[length];
  sb.sgetn(reinterpret_cast<char*>(buff), sizeof(buff));

  for(std::size_t i = 0; i != length; ++i) {
    if(i > 0 && i < 9) continue;
    BOOST_CHECK_EQUAL(buff[i], expected[i]);
  }
}

BOOST_AUTO_TEST_CASE( testLoadAnalogState ) {
  uint8_t expected[] = {
    0x01, 0xd1, 0x44, 0x55, 0x57,
    0x00, 0x00, 0x00, 0x00, 0x90,
    0xf7, 0xaa, 0x95, 0x09, 0xbf,
    0x05, 0x40
  };
  asio::streambuf sb;
  sb.sputn(reinterpret_cast<char*>(&expected), sizeof(expected));
  
  AnalogState as;
  as.load(sb);

  std::size_t length =  sb.size();
  BOOST_CHECK_EQUAL(as.getValue(), 2.71828); // FIXME: This is not good.
  BOOST_CHECK(std::fabs(as.getValue() - 2.71828) < std::numeric_limits<double>::min());
}

BOOST_AUTO_TEST_CASE( testSaveStringState ) {
  uint8_t expected[] = {
    0x03, 0x84, 0x49, 0x55, 0x57, 
    0x00, 0x00, 0x00, 0x00, 0x07, 
    0x00, 0x00, 0x00, 0x32, 0x2e, 
    0x37, 0x31, 0x38, 0x32, 0x38
  };
  StringState ss;
  ss.setValue("2.71828");

  asio::streambuf sb;
  ss.store(sb);

  std::size_t length =  sb.size();
  BOOST_CHECK_EQUAL(length, sizeof(expected));

  uint8_t buff[length];
  sb.sgetn(reinterpret_cast<char*>(buff), sizeof(buff));

  for(std::size_t i = 0; i != length; ++i) {
	  if(i > 0 && i < 9) continue;
	  BOOST_CHECK_EQUAL(buff[i], expected[i]);
  }
}

BOOST_AUTO_TEST_CASE( testLoadStringState ) {
  uint8_t expected[] = {
    0x03, 0x84, 0x49, 0x55, 0x57,
    0x00, 0x00, 0x00, 0x00, 0x07,
    0x00, 0x00, 0x00, 0x32, 0x2e,
    0x37, 0x31, 0x38, 0x32, 0x38
  };
  asio::streambuf sb;
  sb.sputn(reinterpret_cast<char*>(&expected), sizeof(expected));

  StringState ss;
  ss.load(sb);

  BOOST_CHECK_EQUAL(ss.getValue(), "2.71828");
}

BOOST_AUTO_TEST_CASE( testSaveStateEvent ) {
  BooleanState bs;
  MyBooleanListener::Ptr l = boost::make_shared<MyBooleanListener>();
  bs.addChangeListener(l);
  bs.setValue(true);

  BOOST_CHECK_EQUAL(l->isStateChanged(), true);
}

BOOST_AUTO_TEST_CASE( testLoadStateEvent ) {
  uint8_t expected[] = {
    0x02, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x01
  };
  asio::streambuf sb;
  sb.sputn(reinterpret_cast<char*>(&expected), sizeof(expected));
 
  StateEvent se;
  se.load(sb);


  BOOST_CHECK_EQUAL(se.getBefore()->getType(), DI);
  BOOST_CHECK_EQUAL(se.getAfter()->getType(), DI);

  BOOST_CHECK_EQUAL(se.getBefore()->getType(), DI);
  BOOST_CHECK_EQUAL(se.getAfter()->getType(), DI);
}


BOOST_AUTO_TEST_SUITE_END()

