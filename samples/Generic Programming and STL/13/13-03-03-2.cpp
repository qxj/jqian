#include <iostream>
#include <algorithm>
#include <iterator>
using namespace std;

int main()
{
  int A[] = {1, 2, 3, 4, 5, 6};
  const int N = sizeof(A) / sizeof(int);
  
  make_heap(A, A+N);
  int n = N;
  
  while (n > 1) {
    copy(A, A + N, ostream_iterator<int>(cout, " "));
    cout << endl;
    pop_heap(A, A + n);
    --n;
  }
  
  copy(A, A + N, ostream_iterator<int>(cout, " "));
  cout << endl;
}
