#include <iostream>
#include <functional>
#include <iterator>
#include <algorithm>
#include <vector>
using namespace std;

int main()
{
  vector<vector<int> > v;
  v.push_back(vector<int>(2));
  v.push_back(vector<int>(7));
  v.push_back(vector<int>(3));
  
  transform(v.begin(), v.end(),
            ostream_iterator<int>(cout, " "),
            mem_fun_ref(&vector<int>::size));
  cout << endl;
  // ¿é¥X¬° "2 7 3"
}
