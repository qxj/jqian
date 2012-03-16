#include <vector>
#include <algorithm>
#include <functional>
using namespace std;

int main()
{
  vector<int> V1;
  V1.push_back(-2);
  V1.push_back(0);
  V1.push_back(-1);
  V1.push_back(0);
  V1.push_back(1);
  V1.push_back(2);
  
  vector<int> V2;
  remove_copy_if(V1.begin(), V1.end(), back_inserter(V2),
                 bind2nd(less<int>(), 0));
}
