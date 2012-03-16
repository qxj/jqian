#include <iostream>
#include <functional>
#include <algorithm>
#include <iterator>
#include <vector>
using namespace std;

int main()
{
  const int N = 10;
  int A[N] = {1, -3, -7, 2, 5, -9, -2, 1, 6, -8};
  vector<int> V(A, A+N);
  
  sort(V.begin(), V.end(), greater<int>());
  copy(V.begin(), V.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
}
