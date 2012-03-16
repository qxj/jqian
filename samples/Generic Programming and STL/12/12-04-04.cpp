#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
using namespace std;

int main()
{
  vector<double> V;
  V.push_back(1);
  V.push_back(-1);
  V.push_back(-5);
  V.push_back(2);
  
  replace_copy_if(V.begin(), V.end(), ostream_iterator<int>(cout, "\n"),
                  bind2nd(less<int>(), 0), 0);
}
