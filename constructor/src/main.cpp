#include <iostream>
#include <iomanip>

using namespace std;

class Parent {
 public: 
  Parent() : parentMember() { }
  Parent(const Parent& r) : parentMember(r.parentMember) { }
  virtual ~Parent() { }

  Parent& operator=(const Parent& r) { 
    this->parentMember = r.parentMember;
    return *this;
  }

  void setParentMember(int r) {
    this->parentMember = r;
  }

  int getParentMember() const {
    return this->parentMember;
  }

 private:
  int parentMember;
};

class Child : public Parent {
 public:
  Child() : childMember() { }
  Child(const Child& r) : childMember(r.childMember) { }
  virtual ~Child() { }

 Child& operator=(const Child& r) {
   Parent::operator=(r);
   this->childMember = r.childMember;
 }

 void setChildMember(int r) {
   this->childMember = r;
 }
 
 int getChildMember() const {
   return this->childMember;
 }

 private:
  int childMember; 
};

int main(int argc, char* argv[]) {
  Child child1;
  Child child2;
 
  child2.setParentMember(1);
  child2.setChildMember(2);

  cout << "child1.getParentMember() = " << child1.getParentMember() << endl;
  cout << "child1.getChildMember() = " << child1.getChildMember() << endl;
  cout << "child2.getParentMember() = " << child2.getParentMember() << endl;
  cout << "child2.getChildMember() = " << child2.getChildMember() << endl;

  child1 = child2;

  cout << "child1.getParentMember() = " << child1.getParentMember() << endl;
  cout << "child1.getChildMember() = " << child1.getChildMember() << endl;
  cout << "child2.getParentMember() = " << child2.getParentMember() << endl;
  cout << "child2.getChildMember() = " << child2.getChildMember() << endl;
  
  cout << "sizeof(bool) = " << sizeof(bool) << endl;

  return EXIT_SUCCESS;
}
