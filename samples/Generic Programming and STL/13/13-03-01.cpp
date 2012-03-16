#include <iostream>
#include <algorithm>
#include <iterator>
#include <cassert>
using namespace std;

int main()
{
  int A[] = {1, 4, 2, 8, 5, 7};
  const int N = sizeof(A) / sizeof(int);
  
  make_heap(A, A+N);
  assert(is_heap(A, A+N));
  copy(A, A+N, ostream_iterator<int>(cout, " "));
  cout << endl;

  sort_heap(A, A+N);
  assert(is_sorted(A, A+N));
  copy(A, A+N, ostream_iterator<int>(cout, " "));
  cout << endl;
}
