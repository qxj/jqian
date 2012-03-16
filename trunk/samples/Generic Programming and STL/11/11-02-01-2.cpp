#include <iostream>
#include <algorithm>
#include <iterator>
using namespace std;

template <class Integer>
struct congruent {
  congruent(Integer mod) : N(mod) {}
  bool operator()(Integer a, Integer b) const {
    return (a - b) % N == 0;
  }
  
  Integer N;
};

int main()
{
  int A[10] = {23, 46, 81, 2, 43, 19, 14, 98, 72, 51};
  int digits[3] = {1, 2, 3};
  
  int *seq = search(A, A + 10, digits, digits + 3,
                    congruent<int>(10));
  if (seq != A + 10) {
    cout << "Subsequence: ";
    copy(seq, seq + 3, ostream_iterator<int>(cout, " "));
    cout << endl;
  }
  else
    cout << "Subsequence not found" << endl;
  // 輸出結果為：
  // Subsequence: 81 2 43
}
