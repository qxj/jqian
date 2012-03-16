#include <iostream>
#include <functional>
#include <iterator>
#include <algorithm>
#include <vector>
using namespace std;

int main()
{
  vector<vector<int>*> v;
  v.push_back(new vector<int>(5));
  v.push_back(new vector<int>(3));
  v.push_back(new vector<int>(4));
  
  transform(v.begin(), v.end(),
            ostream_iterator<int>(cout, " "),
            mem_fun(&vector<int>::size));
  cout << endl;
  // ¿é¥X¬° "5 3 4"
}
