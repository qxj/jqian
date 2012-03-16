#include <iostream>
#include <vector>
#include <cassert>
using namespace std;

int main()
{
  vector<int> v;
  v.insert(v.begin(), 3);
  assert(v.size() == 1 && v.capacity() >= 1 && v[0] == 3);
}
