#include <iostream>
#include <algorithm>
using namespace std;

int main()
{
  int A1[] = {3, 1, 4, 1, 5, 9, 3};
  int A2[] = {3, 1, 4, 2, 8, 5, 7};
  const int N = sizeof(A1) / sizeof(int);
  
  if (equal(A1, A1 + N, A2))
    cout << "Equal" << endl;
  else
    cout << "Not equal" << endl;
}
