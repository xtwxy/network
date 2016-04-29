#include <cstdlib>
#include <boost/any.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

class Initializer : public boost::enable_shared_from_this<Initializer> {
 public:
  typedef boost::shared_ptr<Initializer> Ptr;
  Initializer() { }
  Initializer(const Initializer& rhs) { }
  Initializer& operator = (const Initializer& rhs) { return *this; }
  virtual ~Initializer() { }

  virtual boost::any getValue() = 0;
};


class Validator : public boost::enable_shared_from_this<Validator> {
 public:
  typedef boost::shared_ptr<Validator> Ptr;
  Validator() { }
  Validator(const Validator& rhs) { }
  Validator& operator = (const Validator& rhs) { return *this; }
  virtual ~Validator() { }

  virtual bool validate(boost::any value) = 0;
};

class Observer : public boost::enable_shared_from_this<Observer> {
 public:
  typedef boost::shared_ptr<Observer> Ptr;
  Observer() { }
  Observer(const Observer& rhs) { }
  Observer& operator = (const Observer& rhs) { return *this; }
  virtual ~Observer() { }

  virtual bool update(boost::any value) = 0;
};

class Codecable {
 public:
  typedef boost::shared_ptr<Codecable> Ptr;
  Codecable() { }
  Codecable(const Codecable& rhs) { }
  Codecable& operator = (const Codecable& rhs) { return *this; }
  virtual ~Codecable() { }

  virtual std::size_t encode(char* bytes, std::size_t offset, int length, 
                             bool& completed) = 0;
  virtual std::size_t decode(char* bytes, std::size_t offset, int length, 
                             bool& completed) = 0;
};


class DataElement {
 public:
  DataElement() { }
  DataElement(const DataElement& rhs) { }
  DataElement& operator = (const DataElement& rhs) { return *this; }
  virtual ~DataElement() { }

  virtual void onInitialize(Initializer& i) = 0;
  virtual void onValidator(Validator::Ptr v) = 0;
  virtual void onUpdate(boost::any value) = 0;
  virtual void addObserver(Observer::Ptr o) = 0;
};


