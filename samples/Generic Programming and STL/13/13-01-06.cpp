#include <algorithm>
#include <functional>
#include <cassert>
using namespace std;

int main()
{
  int A[] = {1, 4, 2, 8, 5, 7};
  const int N = sizeof(A) / sizeof(int);
  
  assert(is_sorted(A, A));
  assert(is_sorted(A, A, greater<int>()));
  
  assert(!is_sorted(A, A + N));
  assert(!is_sorted(A, A + N, greater<int>()));

  sort(A, A + N);
  assert(is_sorted(A, A + N));
  assert(!is_sorted(A, A + N, greater<int>()));

  sort(A, A + N, greater<int>());
  assert(!is_sorted(A, A + N));
  assert(is_sorted(A, A + N, greater<int>()));
}
