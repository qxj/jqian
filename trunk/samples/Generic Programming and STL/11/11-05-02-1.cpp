#include <iostream>
#include <functional>
#include <algorithm>
using namespace std;

int main()
{
  int A1[] = {3, 1, 4, 1, 5, 9, 3};
  int A2[] = {3, 1, 4, 2, 8, 5, 7};
  const int N = sizeof(A1) / sizeof(int);
  
  pair<int*, int*> result = mismatch(A1, A1 + N, A2);
  if (result.first == A1 + N)
    cout << "The two ranges do not differ." << endl;
  else {
    cout << "First mismatch is in position "
         << result.first - A1 << endl;
    cout << "Values: " << *(result.first) << ", "
                       << *(result.second) << endl;
  }
}
