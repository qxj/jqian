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
  
  // �H�V�e�����ǦL�X L �������C
  copy(L.begin(), L.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  
  // �H�A�˪����ǦL�X L �������C
  copy(reverse_iterator<list<int>::iterator>(L.end()),
       reverse_iterator<list<int>::iterator>(L.begin()),
       ostream_iterator<int>(cout, " "));
  cout << endl;
  
  // ��X�Ĥ@�ӼƦr '5'
  list<int>::iterator i1 = find(L.begin(), L.end(), 5);

  // ��X�̫�@�ӼƦr '5'
  list<int>::iterator i2 =
    (find(reverse_iterator<list<int>::iterator>(L.end()),
          reverse_iterator<list<int>::iterator>(L.begin()),
          5)).base();
  --i2;
  assert(*i1 == 5 && *i2 == 5 && distance(i1, i2) == 4);
}
