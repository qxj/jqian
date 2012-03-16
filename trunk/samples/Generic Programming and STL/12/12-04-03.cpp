#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <cassert>
using namespace std;

int main()
{
  vector<int> V1;
  V1.push_back(1);
  V1.push_back(2);
  V1.push_back(3);
  V1.push_back(1);
  
  vector<int> V2(V1.size());
  replace_copy(V1.begin(), V1.end(), V2.begin(), 1, 99);
  assert(V2[1] == V1[1] && V2[2] == V1[2]);
  assert(V2[0] == 99 && V2[3] == 99);
}
