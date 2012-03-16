#include <iostream>
#include <algorithm>
#include <cassert>
using namespace std;

int main()
{
  int A[] = {1, 2, 3, 4, 5, 6, 7};
  const int N = sizeof(A) / sizeof(int);
  
  assert(!is_heap(A, A+N));
  make_heap(A, A+N);
  assert(is_heap(A, A+N));
}
