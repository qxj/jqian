#include <iostream>
#include <iterator>
#include <algorithm>
#include <list>
using namespace std;

int main()
{
  list<int> L;
  L.push_front(3);
  back_insert_iterator<list<int> > ii(L);
  *ii++ = 0;
  *ii++ = 1;
  *ii++ = 2;
  copy(L.begin(), L.end(), ostream_iterator<int>(cout, " "));
  // �L�X�Ӫ��Ȭ� 3 0 1 2
}
