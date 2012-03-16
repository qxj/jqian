#include <iostream>
#include <iterator>
#include <algorithm>
#include <vector>
using namespace std;

int main()
{
  vector<int> V;
  for (int i = 0; i < 20; ++i)
    V.push_back(i);
  reverse_copy(V.begin(), V.end(), ostream_iterator<int>(cout, "\n"));
}
