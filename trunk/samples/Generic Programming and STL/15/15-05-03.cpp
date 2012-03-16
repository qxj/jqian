#include <iostream>
#include <functional>
#include <algorithm>
#include <vector>
#include <cassert>
using namespace std;

int main()
{
  vector<char*> v1(10, (char*) 0);
  vector<int> v2(10, 137);
  vector<int> result(10);
  
  transform(v1.begin(), v1.end(), v2.begin(), result.begin(),
            project2nd<char*, int>());
  assert(equal(v2.begin(), v2.end(), result.begin()));
}
