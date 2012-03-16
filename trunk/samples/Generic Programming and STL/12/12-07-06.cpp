#include <iostream>
#include <iterator>
#include <algorithm>
using namespace std;

int main() {
  int A[] = {2, 3, 4, 5, 6, 1};
  const int N = sizeof(A) / sizeof(int);
  
  cout << "Initially:              ";
  copy(A, A+N, ostream_iterator<int>(cout, " "));
  cout << endl;
  
  prev_permutation(A, A+N);
  cout << "After prev_permutation: ";
  copy(A, A+N, ostream_iterator<int>(cout, " "));
  cout << endl;

  next_permutation(A, A+N);
  cout << "After next_permutation: ";
  copy(A, A+N, ostream_iterator<int>(cout, " "));
  cout << endl;
}
