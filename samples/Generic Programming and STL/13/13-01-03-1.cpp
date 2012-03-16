#include <iostream>
#include <algorithm>
#include <iterator>
using namespace std;

int main()
{
  int A[] = {7, 2, 6, 11, 9, 3, 12, 10, 8, 4, 1, 5};
  const int N = sizeof(A) / sizeof(int);
  
  partial_sort(A, A + 5, A + N);
  copy(A, A + N, ostream_iterator<int>(cout, " "));
  cout << endl;
  // 輸出結果為 "1 2 3 4 5 11 12 10 9 8 7 6"
}
