#include <iostream>
#include <functional>
#include <algorithm>
#include <vector>
using namespace std;

struct B {
  virtual void print() = 0;
};

struct D1 : public B {
  void print() { cout << "I'm a D1" << endl; }
};

struct D2 : public B {
  void print() { cout << "I'm a D2" << endl; }
};

int main()
{
  vector<B*> V;
  
  V.push_back(new D1);
  V.push_back(new D2);
  V.push_back(new D2);
  V.push_back(new D1);
  
  for_each(V.begin(), V.end(), mem_fun(&B::print));
}
