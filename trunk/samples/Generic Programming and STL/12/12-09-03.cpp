#include <iostream>
#include <iterator>
#include <algorithm>
using namespace std;

int main()
{
  const int N = 10;
  int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  
  random_sample_n(A, A+N, ostream_iterator<int>(cout, " "), 4);
  cout << endl;
  // 結果印出來可能為 3 5 6 10，
  //  或任何 209 種其它可能性之一。  
}
