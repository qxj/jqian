#include <iostream>
#include <functional>
#include <algorithm>
#include <vector>
#include <cstdlib>
#include <cassert>
using namespace std;

int main() {
  const int N = 1000;
  vector<double> v(N);
  
  generate(v.begin(), v.end(), rand);
  transform(v.begin(), v.end(), v.begin(),
            bind2nd(divides<double>(), double(RAND_MAX)));
  
  for (int i = 0; i < N; ++i)
    assert(0 <= v[1] && v[i] <= 1.);
}
