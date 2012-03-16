#include <iostream>
#include <functional>
#include <algorithm>
#include <list>
#include <cassert>
#include <cstdlib>
using namespace std;

int main()
{
  list<int> L;
  generate_n(back_inserter(L), 10000, rand);
  
  list<int>::iterator i =
    find_if(L.begin(), L.end(),
            compose2(logical_and<bool>(),
                     bind2nd(greater_equal<int>(), 1),
                     bind2nd(less_equal<int>(), 10)));
  assert(i == L.end() || (*i >= 1 && *i <= 10));
}
