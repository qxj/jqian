#include <iostream>
#include <algorithm>
#include <iterator>
#include <vector>
using namespace std;

int main()
{
  int A[] = {7, 2, 6, 11, 9, 3, 12, 10, 8, 4, 1, 5};
  const int N = sizeof(A) / sizeof(int);
  
  vector<int> V(4);
  partial_sort_copy(A, A + N, V.begin(), V.end());
  copy(V.begin(), V.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  // 輸出結果為 "1 2 3 4"
}
