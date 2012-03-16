#include <iostream>
#include <algorithm>
using namespace std;

int main()
{
  int A1[] = {3, 1, 4, 1, 5, 9, 3};
  int A2[] = {3, 1, 4, 2, 8, 5, 7};
  int A3[] = {1, 2, 3, 4};
  int A4[] = {1, 2, 3, 4, 5};

  const int N1 = sizeof(A1) / sizeof(int);
  const int N2 = sizeof(A2) / sizeof(int);
  const int N3 = sizeof(A3) / sizeof(int);
  const int N4 = sizeof(A4) / sizeof(int);
  
  bool C12 = lexicographical_compare(A1, A1 + N1,
                                     A2, A2 + N2);

  bool C34 = lexicographical_compare(A3, A3 + N3,
                                     A4, A4 + N4);
  
  cout << "A1[] < A2[]: " << (C12 ? "true" : "false") << endl;
  cout << "A3[] < A4[]: " << (C34 ? "true" : "false") << endl;
}
