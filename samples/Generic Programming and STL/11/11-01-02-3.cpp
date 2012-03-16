#include <cassert>
#include <list>
#include <algorithm>
#include <functional>
using namespace std;

int main()
{
  list<int> L;
  L.push_back(-3);
  L.push_back(0);
  L.push_back(3);
  L.push_back(-2);
  L.push_back(7);
  
  list<int>::iterator result = find_if(L.begin(), L.end(),
                                       bind2nd(greater<int>(), 0));
  assert(result == L.end() || *result > 0);
}
