#include <iostream>
#include <iterator>
#include <algorithm>
using namespace std;

int main() {
  const int A[] = {2, 7, 7, 7, 1, 1, 8, 8, 8, 2, 8, 8};
  unique_copy(A, A + sizeof(A) / sizeof(int),
              ostream_iterator<int>(cout, " "));
  cout << endl;
    // ¿é¥X¬° "2 7 1 8 2 8"
}
