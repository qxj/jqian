#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <cassert>
using namespace std;

int main()
{
  vector<double> V;
  V.push_back(1);
  V.push_back(-3);
  V.push_back(2);
  V.push_back(-1);
  
  replace_if(V.begin(), V.end(),
             bind2nd(less<double>(), 0),
             0.);
  assert(V[1] == 0 && V[3] == 0);
}
