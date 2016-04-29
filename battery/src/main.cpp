#include <cstdlib>
#include <map>
#include <restbed>
#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>

#include "BatteryAI.hpp"

using namespace std;
using namespace restbed;

std::string get_ai(int measurer, int battery) {
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
  return ss.str();
}

void get_ai_handler( const shared_ptr< Session > session )
{
    const auto request = session->get_request( );

    size_t content_length = 0;
    std::string connection;

    request->get_header( "Content-Length", content_length );
    request->get_header("Connection", connection);

    int measurer = -1;
    request->get_path_parameter("measurer", measurer, measurer);
    int battery = -1;
    request->get_path_parameter("battery", battery, battery);
 
    if(battery == -1) {
      session->close(NOT_FOUND);
    }

    std::string body = get_ai(measurer, battery);
    std::size_t size = body.size();
    std::multimap<std::string, string> headers = { 
      { "Content-Length",  boost::lexical_cast<std::string>(size) } 
    };
    
    if(boost::iequals(connection, "close") || connection.size() == 0) {
      headers.insert({"Connection", "close"});
      session->close( OK, body, headers);
    } else {
      session->yield( OK, body, headers);
    }
}

int main( const int, const char** )
{
    auto resource = make_shared< Resource >( );
    resource->set_path( "/resource/{measurer: [0-9]*}/{battery: [0-9]*}" );
    resource->set_method_handler( "GET", get_ai_handler);

    auto settings = make_shared< Settings >( );
    settings->set_port( 1984 );
    settings->set_worker_limit(16);
    settings->set_default_header( "Connection", "keep-alive" );

    Service service;
    service.publish( resource );
    service.start( settings );


    return EXIT_SUCCESS;
}

