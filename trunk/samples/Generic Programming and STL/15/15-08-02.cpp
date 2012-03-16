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
    L.push_back(rand() % 4 - 3);
    
  list<int>::iterator first_positive =
         find_if(L.begin(), L.end(),
                 bind2nd(greater<int>(), 0));
  assert(first_positive == L.end() ||  *first_positive > 0);
}
