#include <iostream>
#include <iterator>
#include <algorithm>
#include <functional>
using namespace std;

int main()
{
  const int N = 10;
  double A[N] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  
  transform(A, A + N, A, negate<double>());
  copy(A, A + N, ostream_iterator<double>(cout, "\n"));
}
