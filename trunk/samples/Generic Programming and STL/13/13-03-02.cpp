#include <iostream>
#include <algorithm>
#include <iterator>
using namespace std;

int main()
{
  int A[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  
  make_heap(A, A + 9);
  cout << "[A, A + 9) = ";
  copy(A, A + 9, ostream_iterator<int>(cout, " "));
  cout << endl;

  push_heap(A, A + 10);
  cout << "[A, A + 10) = ";
  copy(A, A + 10, ostream_iterator<int>(cout, " "));
  cout << endl;
}
