#include <iostream>
#include <iterator>
#include <functional>
#include <algorithm>
using namespace std;

int main() 
{
  int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  const int N = sizeof(A)/sizeof(int);
  partition(A, A + N,
            compose1(bind2nd(equal_to<int>(), 0),
                     bind2nd(modulus<int>(), 2)));
  copy(A, A + N, ostream_iterator<int>(cout, " "));
  // ¿é¥X¬° "10 2 8 4 6 5 7 3 9 1"
}
