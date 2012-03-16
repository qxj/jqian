#include <iostream>
#include <iterator>
#include <algorithm>
#include <functional>
#include <memory>
#include <cstdlib>
using namespace std;

class Int {
public:
  Int(int x) : val(x) {}	// Int ¤£¨ã¦³ default constructor
  int get() { return val; }

private:
  int val;
};

int main()
{
  int A1[] = {1, 2, 3, 4, 5, 6, 7};
  const int N = sizeof(A1) / sizeof(int);
  
  Int* A2 = (Int*) malloc(N * sizeof(Int));
  transform(A1, A1 + N,
            raw_storage_iterator<Int*, int>(A2),
            negate<int>());
}
