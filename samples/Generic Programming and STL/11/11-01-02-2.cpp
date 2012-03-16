#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
using namespace std;

int main()
{
  typedef pair<int, char*> Pair;
  
  vector<Pair> V;
  V.push_back(Pair(3, "A"));
  V.push_back(Pair(7, "B"));
  V.push_back(Pair(2, "C"));
  V.push_back(Pair(0, "D"));
  V.push_back(Pair(6, "E"));
  
  vector<Pair>::iterator p =
    find_if(V.begin(), V.end(),
            compose1(bind2nd(equal_to<int>(), 2), select1st<Pair>()));
  cout << "Found: "
       << "<" << (*p).first << "," << (*p).second << ">"
       << endl;
}
