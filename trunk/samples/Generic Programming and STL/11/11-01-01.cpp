#include <list>
#include <algorithm>
#include <cassert>
using namespace std;

int main()
{
  list<int> L;
  L.push_back(3);
  L.push_back(1);
  L.push_back(7);
  
  list<int>::iterator result = find(L.begin(), L.end(), 7);
  assert(result == L.end() || *result == 7);
}
