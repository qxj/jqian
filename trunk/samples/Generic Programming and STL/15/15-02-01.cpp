#include <iostream>
#include <functional>
#include <algorithm>
#include <vector>
#include <cstdlib>
#include <cassert>
using namespace std;

int main()
{
  const int N = 1000;
  vector<double> v1(N);
  vector<double> v2(N);
  vector<double> v3(N);
  
  generate(v1.begin(), v1.end(), rand);
  fill(v2.begin(), v2.end(), -RAND_MAX / 2.);
  
  transform(v1.begin(), v1.end(), v2.begin(), v3.begin(),
            plus<double>());
  
  for (int i = 0; i < N; ++i)
    assert(v3[i] == v1[i] + v2[i]);
}
