#include <iostream>
#include <functional>
#include <algorithm>
#include <vector>
#include <cassert>
using namespace std;

int main()
{
  const int N = 10;
  int A[N] = {-4, -3, 0, -6, 5, -1, -3, 0, 4, -2};
  vector<int> v(A, A+N);
  
  vector<int>::iterator i = find_if(v.begin(), v.end(),
                                    bind2nd(greater_equal<int>(), 0));
  assert(i == v.end() || *i >= 0);
}
