#include <iostream>
#include <algorithm>
using namespace std;

int main()
{
  int A[] = {1, 2, 3, 3, 4, 5};
  const int N = sizeof(A) / sizeof(int);
  
  const int* p = adjacent_find(A, A + N);

  if (p != A + N)
    cout << "Duplicate element: " << *p << endl;
}
