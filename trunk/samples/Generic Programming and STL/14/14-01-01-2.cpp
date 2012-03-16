#include <iostream>
#include <iterator>
#include <algorithm>
#include <list>
#include <vector>
using namespace std;

int main()
{
  vector<int> V(100);
  for (int i = 0; i < 100; ++i)
    V[i] = i;

  list<int> L;
  copy(V.begin(), V.end(), front_inserter(L));
  // L 現在以反向順序容納 V 中所有元素
}
