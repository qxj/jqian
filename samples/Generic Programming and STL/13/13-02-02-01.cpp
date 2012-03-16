#include <iostream>
#include <algorithm>
#include <iterator>
using namespace std;

int main()
{
  int A1[] = { 1, 3, 5, 7 };
  int A2[] = { 2, 4, 6, 8 };
  const int N1 = sizeof(A1) / sizeof(int);
  const int N2 = sizeof(A2) / sizeof(int);
  
  merge(A1, A1 + N1, A2, A2 + N2,
        ostream_iterator<int>(cout, " "));
  cout << endl;
  // ¿é¥X¬° "1 2 3 4 5 6 7 8"
}
