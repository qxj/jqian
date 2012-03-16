#include <iostream>
#include <algorithm>
#include <functional>
using namespace std;

int main()
{
  int A[] = {1, 2, 3, 4, 5, 6, 7, 8};
  const int N = sizeof(A) / sizeof(int);
  
  const int* p = adjacent_find(A, A + N, greater<int>());

  if (p == A + N)
    cout << "The range is sorted in ascending order." << endl;
  else
    cout << "Element " << p - A << " is out of order: "
         << *p << " > " << *(p + 1) << "." << endl;
}
