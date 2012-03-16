#include <iostream>
#include <iterator>
#include <algorithm>
using namespace std;

int main() 
{
  int A[] = {1, 2, 3, 4, 5, 6, 7};
  const int N = sizeof(A) / sizeof(int);
  
  copy(A, A + N, ostream_iterator<int>(cout, " "));
  cout << endl;
                                // 輸出：1 2 3 4 5 6 7
  rotate(A, A + 1, A + N);
  copy(A, A + N, ostream_iterator<int>(cout, " "));
  cout << endl;
                                // 輸出：2 3 4 5 6 7 1
  rotate(A, A + N - 1, A + N);
  copy(A, A + N, ostream_iterator<int>(cout, " "));
  cout << endl;
                                // 輸出：1 2 3 4 5 6 7
  rotate(A, A + 3, A + N);
  copy(A, A + N, ostream_iterator<int>(cout, " "));
  cout << endl;
                                // 輸出：4 5 6 7 1 2 3
}
