#include <iostream>
#include <iterator>
#include <algorithm>
#include <vector>
#include <functional>
using namespace std;

int main()
{
  const int N = 10;
  
  vector<int> V(N);
  fill(V.begin(), V.end(), 75);
  
  int A[N] = {10, 9, 8, 7, 6, 5, 4, 3, 2, 1};

  transform(V.begin(), V.end(), &A[0], 
            ostream_iterator<int>(cout, "\n"),
            plus<int>());
}
