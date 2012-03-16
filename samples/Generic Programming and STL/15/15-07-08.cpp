#include <iostream>
#include <functional>
#include <iterator>
#include <algorithm>
#include <vector>
using namespace std;

struct B {
  virtual int f(int x) const = 0;
};

struct D : public B {
  int val;
  
  D(int x) :  val(x) {}
  int f(int x) const { return val + x; }
};

int main()
{
  vector<D> v;
  
  v.push_back(D(3));
  v.push_back(D(4));
  v.push_back(D(5));

  int A[3] = {7, 8, 9};
  transform(v.begin(), v.end(), A,
            ostream_iterator<int>(cout, " "),
            mem_fun_ref(&B::f));
  cout << endl;
  // ¿é¥X¬° "10 12 14"
}

