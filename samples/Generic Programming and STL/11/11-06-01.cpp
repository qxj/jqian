#include <cassert>
#include <algorithm>
using namespace std;

int main()
{
  const int x = min(3, 9);
  assert(x == 3);
  
  int a = 3;
  int b = 3;
  
  const int& result = min(a, b);
  assert(&result == &a);
}
