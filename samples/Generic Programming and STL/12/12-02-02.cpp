#include <algorithm>
#include <cassert>
using namespace std;

int main()
{
  int x = 1;
  int y = 2;
  assert(x == 1 && y == 2);
  iter_swap(&x, &y);
  assert(x == 2 && y == 1);
}
