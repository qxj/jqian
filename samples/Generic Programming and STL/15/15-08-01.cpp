#include <iostream>
#include <functional>
#include <iterator>
#include <algorithm>
#include <list>
#include <cassert>
using namespace std;

int main()
{
  list<int> L;
  for (int i = 0; i < 20; ++i)
    L.push_back(rand() % 3);
    
  list<int>::iterator first_nonzero =
         find_if(L.begin(), L.end(),
                 bind1st(not_equal_to<int>(), 0));
  assert(first_nonzero == L.end() ||  *first_nonzero != 0);
}
