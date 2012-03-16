#include <iostream>
#include <slist>
#include <algorithm>
#include <iterator>
using namespace std;

int main()
{
  slist<int> L;
  L.push_front(0);
  L.push_front(1);
  L.insert_after(L.begin(), 2);
  copy(L.begin(), L.end(),
       ostream_iterator<int>(cout, " "));
  cout << endl;
}
// ¿é¥X¬°¡G1 2 0
