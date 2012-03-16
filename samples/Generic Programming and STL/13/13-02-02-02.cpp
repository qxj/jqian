#include <iostream>
#include <algorithm>
#include <iterator>
using namespace std;

int main()
{
  int A[] = { 1, 3, 5, 7, 2, 4, 6, 8 };

  inplace_merge(A, A + 4, A + 8);
  copy(A, A + 8, ostream_iterator<int>(cout, " "));
  // ¿é¥X¬° "1 2 3 4 5 6 7 8"
}
