#include <iostream>
#include <algorithm>
#include <vector>
#include <list>
#include <cstring>
using namespace std;

int main()
{
  list<int> L;
  L.push_back(1);
  L.push_back(3);
  L.push_back(5);
  L.push_back(7);
  
  copy(L.begin(), L.end(),
       ostream_iterator<int>(cout, "\n"));
}
