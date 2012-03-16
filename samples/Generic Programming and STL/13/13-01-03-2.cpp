#include <iostream>
#include <algorithm>
#include <iterator>
using namespace std;

int main()
{
  int A[] = {7, 2, 6, 11, 9, 3, 12, 10, 8, 4, 1, 5};
  const int N = sizeof(A) / sizeof(int);
  
  partial_sort(A, A + N, A + N);
  copy(A, A + N, ostream_iterator<int>(cout, " "));
  cout << endl;
  // 輸出結果為 "1 2 3 4 5 6 7 8 9 10 11 12"
}
