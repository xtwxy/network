#ifndef __BATTERY_AI_JSON_
#define __BATTERY_AI_JSON_

#include <vector>
#include <iostream>
#include <boost/foreach.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>

namespace battery {

struct AnalogInputs {
 public:
  AnalogInputs() :
  cellVolts_(),
  batteryVolts_(0),
  batteryCurrent_(0),
  cellTemperatures_() { }
  AnalogInputs(const AnalogInputs& rhs) {
    this->cellVolts_ = rhs.cellVolts_;
    this->batteryVolts_ = rhs.batteryVolts_;
    this->batteryCurrent_ = rhs.batteryCurrent_;
    this->cellTemperatures_ = rhs.cellTemperatures_;
  }
  
  AnalogInputs& operator=(const AnalogInputs& rhs) {
    this->cellVolts_ = rhs.cellVolts_;
    this->batteryVolts_ = rhs.batteryVolts_;
    this->batteryCurrent_ = rhs.batteryCurrent_;
    this->cellTemperatures_ = rhs.cellTemperatures_;

    return *this;
  }

  virtual ~AnalogInputs() { }

  void load(std::istream& stream) {
	assert(false);
    using boost::property_tree::ptree;
    ptree pt;
    read_json(stream, pt);
  }

  void save(std::ostream& stream) {
    using boost::property_tree::ptree;
    ptree pt;

    ptree cellVolts;
    BOOST_FOREACH(double volts, cellVolts_) {
      ptree t;
      t.put("", volts);
      cellVolts.push_back(std::make_pair("", t));
    }
    pt.add_child("cellVolts", cellVolts);

    pt.put("batteryVolts", batteryVolts_);
    pt.put("batteryCurrent", batteryCurrent_);

    ptree cellTemperatures;
    BOOST_FOREACH(double temp, cellTemperatures_) {
      ptree t;
      t.put("", temp);
      cellTemperatures.push_back(std::make_pair("", t));
    }
    pt.add_child("cellTemperatures", cellTemperatures);

    write_json(stream, pt);
  }
  
  std::vector<double> cellVolts_;
  double batteryVolts_;
  double batteryCurrent_;
  std::vector<double> cellTemperatures_;
};



} // namespace battery

#endif //__BATTERY_AI_JSON_

