#include <iostream>
#include <sstream>
#include <boost/test/unit_test.hpp>
#include "BatteryAI.hpp"
#include <random>

BOOST_AUTO_TEST_SUITE(codec);

BOOST_AUTO_TEST_CASE( battery_AnalogInputs_save_test  ) {

  codec::AnalogInputs ai;
  
  std::default_random_engine generator;
  std::uniform_int_distribution<int> volts_distribution(2000,2500);
    
  for(int i = 0; i < 24; ++i) {
    ai.cellVolts_.push_back(volts_distribution(generator)/1000.0);
  }

  std::uniform_int_distribution<int> bvolts_distribution(4800,5600);
  ai.batteryVolts_ = bvolts_distribution(generator)/1000.0;

  std::uniform_int_distribution<int> bcurrent_distribution(-10000,10000);
  ai.batteryCurrent_ = bcurrent_distribution(generator)/1000.0;

  std::uniform_int_distribution<int> temp_distribution(20000,27000);
  for(int i = 0; i < 4; ++i) {
    ai.cellTemperatures_.push_back(temp_distribution(generator)/1000.0);
  }

  std::stringstream ss;
  ai.save(ss);

  std::cout << ss.str() << std::endl;

	BOOST_CHECK( true );
	BOOST_CHECK_EQUAL( true, false );
}

BOOST_AUTO_TEST_SUITE_END();
