#include <iostream>
#include <cstdlib>
#include <list>
#include <algorithm>
using namespace std;

int main()
{
  list<int> L;
  generate_n(front_inserter(L), 1000, rand);
  
  list<int>::const_iterator it = max_element(L.begin(), L.end());
  cout << "The largest element is " << *it << endl;
}
