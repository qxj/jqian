#include <iostream>
#include <functional>
#include <algorithm>
#include <numeric>
#include <vector>
using namespace std;

int main() {
  const int N = 20;
  vector<double> V(N);
  for (int i = 0; i < N; ++i)
    V[i] = i + 1;
  
  partial_sum(V.begin(), V.end(), V.begin(), multiplies<double>());
  copy(V.begin(), V.end(), ostream_iterator<double>(cout, "\n"));
}

