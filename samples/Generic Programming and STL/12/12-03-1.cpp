#include <iostream>
#include <iterator>
#include <algorithm>
#include <list>
#include <functional>
using namespace std;

int main()
{
  const int N = 7;
  double A[N] = {4, 5, 6, 7, 1, 2, 3};
  list<double> L(N);
  
  transform(A, A + N, L.begin(), identity<double>());
  copy(L.begin(), L.end(), ostream_iterator<double>(cout, "\n"));
}
