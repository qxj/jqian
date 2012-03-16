#include <cassert>
#include <slist>
using namespace std;

int main()
{
  slist<int> L;
  L.push_front(0);
  L.push_front(1);
  
  slist<int>::iterator i = L.begin();
  advance(i, 2);
  assert(i == L.end());
}
