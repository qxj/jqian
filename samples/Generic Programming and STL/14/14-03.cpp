#include <iostream>
#include <iterator>
#include <algorithm>
#include <list>
#include <cassert>
using namespace std;

int main()
{
  const int N = 10;
  int A[N] = {2, 5, 7, 8, 1, 5, 3, 6, 9, 1};
  
  list<int> L(A, A+N);
  
  // 以向前的順序印出 L 的元素。
  copy(L.begin(), L.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  
  // 以顛倒的順序印出 L 的元素。
  copy(reverse_iterator<list<int>::iterator>(L.end()),
       reverse_iterator<list<int>::iterator>(L.begin()),
       ostream_iterator<int>(cout, " "));
  cout << endl;
  
  // 找出第一個數字 '5'
  list<int>::iterator i1 = find(L.begin(), L.end(), 5);

  // 找出最後一個數字 '5'
  list<int>::iterator i2 =
    (find(reverse_iterator<list<int>::iterator>(L.end()),
          reverse_iterator<list<int>::iterator>(L.begin()),
          5)).base();
  --i2;
  assert(*i1 == 5 && *i2 == 5 && distance(i1, i2) == 4);
}
