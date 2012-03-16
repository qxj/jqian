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
  
  vector<bool> v1;
  for (int i = 0; i < N; ++i)
    v1.push_back(rand() > (RAND_MAX / 2));
    
  vector<bool> v2;
  transform(v1.begin(), v1.end(), back_inserter(v2),
            logical_not<bool>());
  
  for (int i = 0; i < N; ++i)
    assert(v1[i] == !v2[i]);
}
