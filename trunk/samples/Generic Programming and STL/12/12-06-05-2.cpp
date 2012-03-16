#include <iostream>
#include <iterator>
#include <functional>
#include <algorithm>
using namespace std;

int main() {
  int A[8] = {7, 7, 1, 4, 6, 6, 6, 3};
  
  int *new_end = unique(A, A + 8, equal_to<int>());
  copy(A, new_end, ostream_iterator<int>(cout, "\n"));
}
