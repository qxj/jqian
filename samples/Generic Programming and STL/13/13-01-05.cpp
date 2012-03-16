#include <iostream>
#include <algorithm>
#include <iterator>
using namespace std;

int main()
{
  int A[] = {7, 2, 6, 11, 9, 3, 12, 10, 8, 4, 1, 5};
  const int N = sizeof(A) / sizeof(int);
  
  nth_element(A, A + 6, A + N);
  copy(A, A + N, ostream_iterator<int>(cout, " "));
  cout << endl;
}
