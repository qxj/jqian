#include <algorithm>
#include <cassert>
#include <vector>
using namespace std;

int main()
{
  vector<int> V1;
  V1.push_back(1);
  V1.push_back(2);
  
  vector<int> V2;
  V2.push_back(3);
  V2.push_back(4);
  
  assert(V1[0] == 1 && V1[1] == 2 && V2[0] == 3 && V2[1] == 4);
  swap_ranges(V1.begin(), V1.end(), V2.begin());
  assert(V1[0] == 3 && V1[1] == 4 && V2[0] == 1 && V2[1] == 2);
}
