#include <iostream>
#include <functional>
#include <algorithm>
#include <vector>
#include <cassert>
using namespace std;

int main()
{
  vector<int> v1(10, 137);
  vector<char*> v2(10, (char*) 0);
  vector<int> result(10);
  
  transform(v1.begin(), v1.end(), v2.begin(), result.begin(),
            project1st<int, char*>());
  assert(equal(v1.begin(), v1.end(), result.begin()));
}
