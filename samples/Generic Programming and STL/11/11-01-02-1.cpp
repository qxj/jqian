#include <iostream>
#include <algorithm>
#include <functional>
using namespace std;

int main()
{
  int A[] = {4, 1, 0, 3, 2, 0, 6};
  const int N = sizeof(A) / sizeof(int);
  
  int* p = find_if(A, A + N,
                   bind2nd(equal_to<int>(), 0));

  cout << "Index of first zero = " << p - A << endl;
}
