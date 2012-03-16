#include <iostream>
#include <functional>
#include <algorithm>
#include <vector>
#include <cstdlib>
#include <cassert>
using namespace std;

int main() {
  const int N = 1000;
  vector<int> v1(N);
  vector<int> v2(N);
  
  generate(v1.begin(), v1.end(), rand);
  transform(v1.begin(), v1.end(), v2.begin(),
            negate<int>());
  
  for (int i = 0; i < N; ++i)
    assert(v1[i] + v2[i] == 0);
}
