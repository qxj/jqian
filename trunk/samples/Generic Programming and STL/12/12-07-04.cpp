#include <iostream>
#include <iterator>
#include <algorithm>
using namespace std;

int main()
{
  int A[] = {1, 1, 1, 2, 2, 2, 2};
  const int N = sizeof(A) / sizeof(int);
  
  rotate_copy(A, A + 3, A + N, ostream_iterator<int>(cout, " "));
  cout << endl;
                                // ¿é¥X¡G2 2 2 2 1 1 1
}
