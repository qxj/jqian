#include <iostream>
#include <functional>
#include <algorithm>
#include <iterator>
#include <list>
using namespace std;

int main()
{
  const int N = 10;
  int A[N] = {3, -7, 0, 6, 5, -1, -3, 0, 4, -2};
  
  list<int> L;
  
  remove_copy_if(A, A+N, back_inserter(L),
                 bind2nd(less_equal<int>(), 0));
  
  cout << "Elements in list: ";
  copy(L.begin(), L.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
}
