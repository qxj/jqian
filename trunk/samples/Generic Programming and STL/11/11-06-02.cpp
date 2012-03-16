#include <cassert>
#include <algorithm>
using namespace std;

int main()
{
  const int x = max(3, 9);
  assert(x == 9);
  
  int a = 3;
  int b = 3;
  
  const int& result = max(a, b);
  assert(&result == &a);
}
