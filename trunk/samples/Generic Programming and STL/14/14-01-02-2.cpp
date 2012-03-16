#include <iostream>
#include <iterator>
#include <algorithm>
#include <list>
#include <vector>
#include <cassert>
using namespace std;

int main()
{
  list<int> L;
  for (int i = 0; i < 100; ++i)
    L.push_back(i);

  vector<int> v1(L.begin(), L.end());
  vector<int> v2;
  copy(L.begin(), L.end(), back_inserter(v2));
  
  assert(v1 == v2);
}
