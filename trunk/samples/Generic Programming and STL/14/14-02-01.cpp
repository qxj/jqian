#include <iostream>
#include <iterator>
#include <algorithm>
#include <vector>
using namespace std;

int main()
{
  vector<int> V;
  copy(istream_iterator<int>(cin), istream_iterator<int>(),
       back_inserter(V));
}
