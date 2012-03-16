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
  V.push_back(Pair(3, "D"));
  V.push_back(Pair(0, "E"));
  V.push_back(Pair(6, "E"));
  
  cout << "Number of elements with key equal to 3: "
       << count_if(V.begin(), V.end(),
                   compose1(bind2nd(equal_to<int>(), 3),
                            select1st<Pair>()))
       << endl;
}
