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
  vector<D1> V;
  
  V.push_back(D1());
  V.push_back(D1());
  
  for_each(V.begin(), V.end(), mem_fun_ref(&B::print));
}
