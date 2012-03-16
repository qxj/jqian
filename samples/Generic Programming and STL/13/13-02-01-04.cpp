#include <iostream>
#include <algorithm>
using namespace std;

int main() {
  int A[] = { 1, 2, 3, 3, 3, 5, 8 };
  const int N = sizeof(A) / sizeof(int);
  
  for (int i = 2; i <= 5; ++i) {
    pair<int*, int*> result = equal_range(A, A + N, i);
  
    cout << endl;
    cout << "Searching for " << i << endl;
    
    cout << "  First position where " << i << " could be inserted: "
         << result.first - A << endl;
    cout << "  Last position where " << i << " could be inserted: "
         << result.second - A << endl;
    
    if (result.first < A + N)
      cout << "  *result.first = " << *result.first << endl;
    if (result.second < A + N)
      cout << "  *result.second = " << *result.second << endl;
  }
}
