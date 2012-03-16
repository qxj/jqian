#include <iostream>
#include <iterator>
#include <algorithm>
#include <vector>
using namespace std;

struct eq_div {
  eq_div(int divisor) : n(divisor) {}
  bool operator()(int x, int y) const { return x / n == y / n; }

  int n;
};

int main() {
  int A[] = {1, 5, 8, 15, 23, 27, 41, 42, 43, 44, 67, 83, 89};
  const int N = sizeof(A) / sizeof(int);
  
  vector<int> V;
  unique_copy(A, A + N, back_inserter(V), eq_div(10));
  
  copy(V.begin(), V.end(), ostream_iterator<int>(cout, "\n"));
  // ¿é¥X¬° 1 15 23 41 67 83
}
