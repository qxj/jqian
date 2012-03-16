#include <iostream>
#include <algorithm>
using namespace std;

int main() {
  int A[] = { 1, 2, 3, 3, 3, 5, 8 };
  const int N = sizeof(A) / sizeof(int);
  
  for (int i = 1; i <= 10; ++i) {
    cout << "Searching for " << i << ": "
         << (binary_search(A, A + N, i) ? "present" : "not present")
         << endl;
  }
}
