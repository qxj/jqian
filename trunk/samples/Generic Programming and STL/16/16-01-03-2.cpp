#include <iostream>
#include <slist>
#include <algorithm>
#include <iterator>
using namespace std;

int main() {
  slist<double> L;
  double x = 1;
  L.push_front(x);
  slist<double>::iterator back = L.begin();
  
  while (x < 1000000.)
    back = L.insert_after(back, x *= 2);
  copy(L.begin(), L.end(),
       ostream_iterator<double>(cout, "\n"));
}
// 輸出為從 1 到 1048576 的二次方。
