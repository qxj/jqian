#include <iostream>
#include <algorithm>
#include <iterator>
using namespace std;

int main()
{
  int A[15] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  copy_backward(A, A + 10, A + 15);
  
  copy(A, A + 15, ostream_iterator<int>(cout, " "));
  cout << endl;
  // ¿é¥X¬° "1 2 3 4 5 1 2 3 4 5 6 7 8 9 10"¡C
}
