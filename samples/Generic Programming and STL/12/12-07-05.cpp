#include <iostream>
#include <iterator>
#include <algorithm>
using namespace std;

int main() {
  const int N = 4;
  int A[N] = {1, 2, 3, 4};
  
  do {
    copy(A, A + N, ostream_iterator<int>(cout, " "));
    cout << endl;
  } while (next_permutation(A, A + N));
}
