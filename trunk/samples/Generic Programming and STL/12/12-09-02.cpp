#include <iostream>
#include <iterator>
#include <algorithm>
using namespace std;

int main()
{
  const int N = 10;
  const int n = 4;
  int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  int B[n];
  
  random_sample(A, A+N, B, B+n);
  copy(B, B + n, ostream_iterator<int>(cout, " "));
  cout << endl;
  // 結果印出來可能為 1 6 3 5，
  //  或任何 5039 種其它可能性之一。  
}
